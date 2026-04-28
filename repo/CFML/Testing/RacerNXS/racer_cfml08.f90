!!----
!!---- Program: Racer
!!----   Info: Program Used For The Integration Of Bragg Peak.
!!----
!!----   This program has been written by Clive Wilkinson and the algorithms used in it
!!----   are described in the paper:
!!----   Integration of Single-Crystal Reflections Using Area Multidetectors
!!----   by Clive Wilkinson, Hanna W. Khamis, Robert F. D. Stansfield and Garry J. Mcintyre
!!----   Journal of Applied Crystallography 21, 471-478 (1988)
!!----
!!----
!!---- History
!!----    Update: March   - 2018  (J. Rodriguez-Carvajal) handle up to 64 x 64 pixels
!!----    Update: January - 2010  (Eric Pellegrini)
!!----
!!--..    Some Code Cleaning:
!!--..        - Unused Variables Removed
!!--..        - Only The Necessary Variables/Functions/Procedures Of The Different Modules Are Declared
!!----
!!----    Update: December - 2006
!!----
!!----    Code converted from f77 to f90 using To_F90 by Alan Miller. Refactoring
!!----    done by J. Rodriguez-Carvajal in December 2006
!!----
!!
 Program Racer_CFML

    Use CFML_Globaldeps,   Only: Pi, To_Deg, To_Rad, Cp, Dp, Err_CFML, Clear_Error

    Use CFML_Maths,        Only: Inverse_Matrix, Diagonalize_Sh

    Use CFML_Messages,     Only: Error_Message

    Use Racer_CFML_Mod,    Only: Backgr, Calpos, Config, Display, Dx, Dy, Elplim, Expub, Flag,      &
                                 Graph, I_Ans, I_Flg, I_Fly, I_Lib, I_Raf, I_Ubm, Libent, Mask,I_Ell,    &
                                 Max_Pts, Plan, Powder, Refine, Results, Stang, Vraw, Vval,         &
                                 Zgamma, Zomega, Zmon, Ztim, Numors, n_ver, n_hor, display_ij

    Use Racer_CFML_Files,  Only: Racer_File_Error, Racer_File_Error_Mess, Openf2, Race_Read_Scan  , &
                                 Race_Setup, Snum, Cfg_Basename, Map, Npb,Bgp, Bgerr

    ! Required to save Numor int vtk (vts) format
    Use CFML_Metrics,         Only: Cell_G_Type, Set_Crystal_Cell
    Use CFML_Export_Vtk,      Only:  array3D_pointdata_2_vts
    Implicit None

    Character(Len=10)                 :: Atype, filin
    Character(Len=80)                 :: Rafbuf
    Real, Dimension(Max_Pts)          :: Ftot, Cnt, Cnterr, Rmon, Frame
    Integer, Dimension(Max_Pts)       :: Nftot
    Real,    Dimension(3,3)           :: Tens, Elps, Dircos, Socrid, Temp, Prin,         &
                                         Procel, Tmat, Phim, Chim, Omem, Gamm, Pmet, Prod,       &
                                         Shear, Elplib, Scal,  Twinel, Twtmat

    Real,    Dimension(3)             :: H, Hproc, Dh, Dh1, Dh2, Dh3, Dh4, Dh5, Dh6, Dht, Rht
    Real,    Dimension(2)             :: Dfds,Suma
    Real,    Dimension(3)             :: Valu, Xpos, Vol, Cg, Delang, Dpt, Dp0, Dp1, Dp2, Dp3,      &
                                         Dp4, Dp5, Dp6, vtemp
    Real,    Dimension(12)            :: Dne, Dpar
    Real,    Dimension(13)            :: Pk
    Real,    Dimension(12,12)         :: Square, Cne, Enc
    Real,    Dimension(1000,13)       :: Pklib
    Integer, Dimension(2)             :: Ntot
    Integer, Dimension(3)             :: Ihi, Maxx, Minn
    Integer, Dimension(17)            :: Jflag
    Logical                           :: Singular
    Integer                           :: Ier, I, J, K, Kfmin, Kfmax, N, M, L, Ikmin, Ikmax, Kftot,  &
                                         Nppts, Nbgpts, Newppts, Ifw, Mass, Kk, Jj, Ii, Iii, Jjj,   &
                                         Irange, Jrange, Krange, Kcen, Ncol, Itype, Izobs, Iinc,    &
                                         Iub, Nlib, Machine, Numor_Ind
    Real                              :: Diff, Ct, Scantot, Bg, Pksum, Pkthresh, Peak, Perr, Bgpp,  &
                                         Appbgpp, Bgpperr, Sigtonoise, Peakapp, Frac, Fracerr, Sh,  &
                                         Elpvol, Ftotmax, Fht, Fwhh, Thresh, Xobs, Step, Rnu, Rnum, &
                                         Xcalc, Ycalc, Zcalc, Dgamdom, Dzomega, Dz, Drnu, Domega,   &
                                         Dgamma, Dp1Sq, Dp2Sq, Dp3Sq, Dp4Sq, Dp5Sq, Dp6Sq, Dptsq,   &
                                         Stvol, Ffrac, Smon, Ctmax, Xp3, Xp1, Xp2,Yobs,Zobs, Trace, &
                                         Rsq, Ratio, Frptz, Gamobs, Omobs, Anglev, Peak1, Prh,      &
                                         Sumsq, Phihead, Chihead, Omegahead, Gammahead, Phi, Chi,   &
                                         Omega, Gamma, Temper
    Real (Kind=Dp)                    :: Time_Start, Time_End
    type(Cell_G_Type) :: cell
    real(kind=Cp), dimension(3) :: cell_a, cell_alpha
    character(len=80) vtk_fil
    logical :: write_vtk=.false.
    !Variables for debugging
    integer :: i_debug=43

    Stang=0.05 !constant through all the program, used for step normalisation
    Dx=2.0     !Separation between wires in mm
    Dy=2.0     !
!-------------------------------------------------------------------
!       Set Up Invariant Elements In Phi,Chi,Omega,Gamma,H Matrices
!-------------------------------------------------------------------
    Call Cpu_Time(Time_Start)

    Phim(:,:) = 0.0
    Chim(:,:) = 0.0
    Omem(:,:) = 0.0
    Gamm(:,:) = 0.0
    H(:)      = 0.0
    Phim(3,3) = 1.0
    Chim(2,2) = 1.0
    Omem(3,3) = 1.0

!-------------------------------------------------------------------
!       Initialise Matrices for UB Refinement
!-------------------------------------------------------------------
    Iub=0                    ! Initialise Peak Count
    Sumsq=0.0                !     "   Sum Of (Ang Diff)**2
    Dne(:)=0.0               !     "      Differences
    Dpar(:)=0.0
    Cne(:,:)=0.0             !     "   Coefficients In Normal Eqs

!-------------------------------------------------------------------
!       Open files and read parameters
!-------------------------------------------------------------------
    Call Race_Setup()

    !Opening the debug File
    open(unit=i_debug, file="debug.inf",status="replace",action="write")
!--------------------------------------------------------------------
!- Calculate the twin transformation matrix if necessary
!--------------------------------------------------------------------
    If (Config%Twin_UB_Set) Then

        !Call Invert_Matrix(Config%Twin_UB,Twinel,Singular) ! Get Inverse Of Twin Ub
        singular=.false.
        Twinel=Inverse_Matrix(Config%Twin_UB)
        if(err_CFML%Ierr /= 0) then
            singular=.true.
            write(*,"(a)") " => "//trim(err_CFML%Msg)
            call Clear_Error()
        end if
        Twtmat= Matmul(Twinel,Config%UB)                   ! [Twin Ub]-1.[Proc Ub]

        Write(*,    225) ((Twtmat(I,J),J=1,3),I=1,3)
        Write(I_Ans,225) ((Twtmat(I,J),J=1,3),I=1,3)
        Write(I_Flg,225) ((Twtmat(I,J),J=1,3),I=1,3)
        225 Format(/, ' Masking of Reflections from a Twin Included.', &
                   /, ' Transformation Matrix between the Orientation Matrices: ',  &
                   /,15X,'[',3F10.6,']',/,' [Twin Ub]-1 = [',3F10.6,'][Ub]-1',  &
                   /,15X,'[',3F10.6,']',/)
    End If

    Write(I_Flg,222)
    Write(*,    222)
    222 Format(2X, 'Flags Are:', &
                /, ' F1 - Strong Peak  F2 - Strong Peak Relegated to Weak Channel',  &
                /, ' F3 - Bad Egg  F4 - Fried Egg', /,' F5  - Uneven Background',  &
                /, ' F6 - Calculated and Observed Peak Centres differ by more' )
    Write(I_Flg,223) Config%DiffLim,Config%Imin,Config%Imax,Config%Jmin,Config%Jmax, &
                     Config%Kmin,Config%Kmax
    Write(*,    223) Config%DiffLim,Config%Imin,Config%Imax,Config%Jmin,Config%Jmax, &
                     Config%Kmin,Config%Kmax
    223 Format(1X,'     than  ',F5.2,' Deg.',/,  &
        ' F7 - Process and Measurement Wavelengths differ.',/,  &
        ' F8 - Process with Measurement UB-Matrix.',/,  &
        ' F9,F12  - Integration Points for Peak Requested Outside',/,  &
        '           Nu Data Limits. (Rows',I3,' To',I3,')',/,  &
        ' [F10,F13 - Gamma (+,-) (Columns',I3,' To',I3,')',/,  &
        ' F11,F14 -  Omega (-,+) (Frames',I3,' To',I3,')]',/,  &
        ' F15 - Data Points in peak have been Masked Out',/,  &
        ' F16 - Rotation Angle Sticking',/, ' F17 - Calculated Centre Outside Data',/)
    Write(I_Flg,"(2X,A,16I4)") 'Numor  F1',(J,J=2,17)
    Write(*,    "(2X,A,16I4)") 'Numor  F1',(J,J=2,17)

!--------------------------------------------------------------------
!       Read from Peak Library File (Delete before writing new library)
!--------------------------------------------------------------------
    Nlib=0
    Do While (Nlib < 1000)
        I=Nlib+1
        Read(I_Lib,"(F7.0,3F9.3,6G10.3,3F9.3)",Iostat=Ier) (Pklib(I,J),J=1,13)       ! Read Library.
        If (Ier /= 0) Exit
        Nlib=Nlib+1
    End Do

    If (.Not. Config%UB_Set)  Jflag(14) = 1
    If (Config%Wavelen >= 0)  Jflag(13) = 1

!--------------------------------------------------------------------
!       Start of next reflection cycle
!--------------------------------------------------------------------
! Open a file to store the ellipsoidal parameters
    open(unit=I_Ell,file=trim(Cfg_Basename)//".ell",status='replace')
!
   do_Ref: Do Numor_Ind = 1, Size(Numors)

        Jflag( 1:12)=0                        ! Reset Error Flags
        Jflag(15:17)=0

        If (Config%IRefine >= 0) Then

            If (Iub > Max(12,Config%IRefine-1)) Then
                Do I=1,12  !What is this 12? => 9 components of UB-matrix + 3 offsets
                    Do J=1,I
                        Cne(I,J)=Cne(J,I)
                    End Do
                End Do

                ! UB Refinement Every Refine
                !Call Invert_Matrix(Cne,Enc,Singular)
                singular=.false.
                Enc=Inverse_Matrix(Cne)
                if(err_CFML%Ierr /= 0) then
                    singular=.true.
                    write(*,"(a)") " => "//trim(err_CFML%Msg)
                    call Clear_Error()
                end if

                ! (Or More!)
                If (.Not. Singular) Then
                    ! Strong Reflections
                    Call Refine(Cne,Dne,Dpar,Machine)
                    ! Give Quality Factor
                    Write(I_Ubm,"(F10.4,t65,a)") To_Deg*Sqrt(Sumsq/Iub),"! Quality factor: To_Deg*Sqrt(Sumsq/Iub)"
                    ! Numor Of Last Strong Peak
                    Write(I_Ubm,"(/,1X,I6,t65,a,/)") Numors(Numor_Ind), "! Numor of Last Strong Peak"
                    ! Reset Strong Peak Count
                    Iub=0
                    !   "  Sums ( Diff)**2
                    Sumsq=0
                End If
            End If
        End If

        ! Read raw data for each reflection.
        Call Race_Read_Scan(Numors(Numor_Ind))

        If (Racer_File_Error) Then
            Write(*,"(A)") Trim(Racer_File_Error_Mess)
            Racer_File_Error = .False.
            Racer_File_Error_Mess = " "
            Cycle do_Ref
        End If

        ! Ignore Acdq'S.
        If (Snum%Nframes < 2) Then
            Write(*,"(A,1X,I6.6,1X,A)") '!!! The Number of Frames is < 2. Numor', Numors(Numor_Ind), 'Skipped !!!'
            Cycle do_Ref
        End If

        ! Ignore Scans With No Step.
        If (Snum%Scans(2) <= 0.0) Then
            Write(*,"(A,1X,I6.6,1X,A)") '!!! The scan has no Step. Numor', Numors(Numor_Ind), 'Skipped !!!'
            Cycle do_Ref
        End If

        ! H,K,L Values
        H(:)=Snum%Hmin(:)
        !write(*,"(i8,3f10.2)") Numors(Numor_Ind),H

        ! Pick Up Header Of Phi.
        Phihead=Snum%Angles(1)

        ! Pick Up Header Of Chi.
        Chihead=Snum%Angles(2)

        ! Pick Up Header Of Omega.
        Omegahead=Snum%Angles(3)

        ! Pick Up Header Of Gamma.
        Gammahead=Snum%Angles(4)

        Phi   = Phihead
        Chi   = Chihead
        Omega = Omegahead
        Gamma = Gammahead

        ! Sample Temperature.
        Temper=Snum%Conditions(3)

        ! Regulation Temperature.
        If (Config%TReg == 1 .Or. Temper == -1 .Or. Temper > 9000) Temper=Snum%Conditions(2)

        Expub(:,:)= Snum%Ub(:,:)           ! UB Matrix at time of experiment.

        If (.Not. Config%UB_Set) Config%UB(:,:)=Expub(:,:)

        !write(*,"(a,i8)") "  UB-matrices for numor (Expub, Snum%UB): ", Numors(Numor_Ind)
        !do j=1,3
        !   write(*,"(3f14.7,a,3f14.7)") Expub(j,:),"    ",Config%UB(j,:)
        !end do

        !!!---------------------------------------------------------------------
        !!!       Re-index and nudge or reject reflections
        !!!---------------------------------------------------------------------
        !Call Invert_Matrix(Config%UB,Procel,Singular) ! Get the inverse of UB matrix
        singular=.false.
        Procel=Inverse_Matrix(Config%UB)
        if(err_CFML%Ierr /= 0) then
            singular=.true.
            write(*,"(a)") " => "//trim(err_CFML%Msg)
            call Clear_Error()
        end if
        Tmat = Matmul(Procel,Expub)                   !  "  Transformation Matrix (identity if Expub=Config%UB)
        Hproc= Matmul(Tmat,H)                         !  "  Varied Indices (=hkl if Expub=Config%UB)

            Dh=H-Hproc                ! Difference in indices from measurement
            Diff=dot_product(Dh,Dh)   !

        !write(*,"(2(a,3f6.2),3x,f8.4)") " H: ",H, "  Hproc: ",Hproc, sqrt(Diff)
        Iinc=Config%Icard             ! Put Iinc to process real hkl (incommensurate or twinned crystals)

        If (Sqrt(Diff) < 0.2) Then    ! If the shift in indices is small just nudge the reflection

            Hproc=H                   ! Nudge only

        Else
                                        ! Transformed indices:
            Dh=Real(Nint(Hproc))-Hproc  ! Find Diffs from integer Hkl
            Diff=dot_product(Dh,Dh)


            If (Sqrt(Diff) > 0.5) Then

                If (Iinc >= 1) Then
                                        ! Treat as Incommensurate,
                        Dh =0.0         ! with reflections in
                                        ! original positions, but with transformed indices.
                    Write(I_Ans,"(/,A,/)") ' **** N.B. Incommensurate Transformation ****'            !
                Else
                    Write(I_Ans,"(/,A,/)") ' **** Non-Integral Indices.  Ignore Scan ****'            ! Ignore Scan
                    Jflag(17)=1
                    Cycle do_Ref
                End If
            Else
                Write(I_Ans,"(/,a,3F7.2,a,3f7.2,a/)") ' **** Large Delta-H: H, Hproc :  (',H,')  (', Hproc,')'
                Hproc = Real(Nint(Hproc))

            End If
        End If

        !!!-------------------------------------------------------------------
        !!!       Write(I_Ans,"(3F12.7)") ((Expub(I,J),J=1,3),I=1,3)
        If (Config%Wavelen <= 0) Config%Wavelen=Snum%Wave ! Valco(18)  ! Pick Up Lamda.

        Step=Snum%Scans(2) !Valdef(2)

        If (Abs(Snum%Icalc) == 2) Then      ! Normal Beam Calcs
            Machine=1
            Rnu=Chi                         ! Will eventually be changed
            Rnum=Chi                        ! Store the original Rnu
            Chi=0.0                         !
        Else                                ! 4 Circle Calcs
            Machine=0
            Rnu=0.0
            Rnum=0.0
        End If
                      ! X,Y Offsets are in mm, divide by Dx,Dy to get pixels
        !Xcalc=15.5+(Config%XOffset/Dx)
        Xcalc=0.5*real(n_hor-1)+(Config%XOffset/Dx)        ! Calculation in pixels, must be modified if the detector
        !Ycalc=15.5+(Config%YOffset/Dy)
        Ycalc=0.5*real(n_ver-1)+(Config%YOffset/Dy)        ! has more or less than 32 x 32 (or 64 x 64) pixels
        Zcalc=(Snum%Nframes/2)+1+(Config%OmegaOffset/Step) ! omega-offset and step are in degrees

        If (Config%Kmax > Snum%Nframes) Then
            Config%Kmax=Snum%Nframes
            Write(*,*) ' Data Requested Beyond Final Step'
            Write(I_Ans,*) ' Data Requested Beyond Final Step'
        End If

        Dgamdom=(Zgamma(Config%Kmax)-Zgamma(1))  !domain in gamma (degrees)
        Dzomega=(Zomega(Config%Kmax)-Zomega(1))  !domain in omega (degrees)

        If (Abs(Dzomega) < 0.001) Then
            Write( *,"(/,2X,A,I6,A)")    'No Variation In Omega For Numor', Numors(Numor_Ind),' Integration Abandoned.'
            Write(I_Ans,"(/,2X,A,I6,A)") 'No Variation In Omega For Numor', Numors(Numor_Ind),' Integration Abandoned.'
            Cycle do_Ref
        End If

        Dgamdom=Dgamdom/Dzomega   !Should be equal to gamma-omega coupling factor
        !!!-
        !!!- Check that observed and pre-calculated coupling factor are
        !!!- in the same tennis court
        If (Abs(Dgamdom-Snum%Cpl_Fact) > 0.05) Then
            Write( *,'(A)')    ' Attn: Calc/Obs Coupling Factor Different By More Than 5%'
            Write(I_Ans,'(A)') ' Attn: Calc/Obs Coupling Factor Different By More Than 5%'
            Dgamdom=Snum%Cpl_Fact  !use Dgamdom as given in the numor
        End If

        !!!       ****Check that scan angle has changed****
        Do K = Config%Kmin, Config%Kmax-1
            Dz = (Zomega(K+1)-Zomega(K))
            If (Abs(Dz) < 0.001) Then
                Write( *,"(/,2X,A,I6,A,I3)") 'No variation in Omega for Numor ',Numors(Numor_Ind),' Step',K
                Write(I_Ans,"(/,2X,A,I6,A,I3)") 'No variation in Omega for Numor ',Numors(Numor_Ind),' Step',K
                Jflag(16) = 1
            End If
        End Do

        Kk=0
        Do K=Config%Kmin,Config%Kmax
            Kk=Kk+1
            Jj=0
            !- This Is +1 because we say that detector is (0-31, 0-31) or (0-63, 0-63)
            Do J=Config%Jmin+1,Config%Jmax+1
                Jj=Jj+1
                Ii=0
                Do I=Config%Imin+1,Config%Imax+1
                    Ii=Ii+1
                    Vval(Ii,Jj,Kk)=Vraw(I,J,K)  !Copy the used section of Vraw into Vval
                End Do
            End Do
        End Do

        if (write_vtk .and. Config%Racer_nexus) then
          cell_a=[n_ver,n_hor,Snum%Nframes]
          cell_alpha=[90.0,90.0,90.0]
          call Set_Crystal_Cell(cell_a,cell_alpha,cell)
          write(unit=vtk_fil,fmt='(I12)') Numors(Numor_ind)
          call array3D_pointdata_2_vts(reshape(Snum%counts(:,:),[n_ver,n_hor,Snum%Nframes]),cell,0.0,1.0,0.0,1.0,0.0,1.0,trim(vtk_fil))
        end if
        !!!-------------------------------------------------------------------
        !!!       Use change in UB matrix to predict peak shifts
        !!!-------------------------------------------------------------------
        Phim(1,1)= Cosd(Phi)
        Phim(2,2)= Phim(1,1)                     ! Phi Rotation
        Phim(1,2)= Sind(Phi)
        Phim(2,1)=-Phim(1,2)
        Chim(1,1)= Cosd(Chi)
        Chim(3,3)= Chim(1,1)                     ! Chi Rotation
        Chim(1,3)= Sind(Chi)
        Chim(3,1)=-Chim(1,3)
        Omem(1,1)= Cosd(Omega)
        Omem(2,2)= Omem(1,1)                     ! Omega Rotation
        Omem(1,2)= Sind(Omega)
        Omem(2,1)=-Omem(1,2)
        Gamm(1,1)= Cosd(Rnu)*Cosd(Gamma)
        Gamm(1,2)= 1-Gamm(1,1)
        Gamm(2,2)= Cosd(Rnu)*Sind(Gamma)
        Gamm(2,1)=-Gamm(2,2)
        Gamm(1,3)=-Sind(Rnu)*Sind(Gamma)
        Gamm(2,3)=-Sind(Rnu)*Cosd(Gamma)
        Gamm(3,3)= Cosd(Rnu)

        !Call Invert_Matrix(Gamm,Temp,Singular)  ! Gamma-1
        singular=.false.
        Temp=Inverse_Matrix(Gamm)
        if(err_CFML%Ierr /= 0) then
            singular=.true.
            write(*,"(a)") " => "//trim(err_CFML%Msg)
            call Clear_Error()
        end if

        Gamm=Temp                         ! Loaded To Gamm
        Pmet=Matmul(Chim,Phim)            ! [Chi]*[Phi]
        Temp=Matmul(Omem,Pmet)            ! [Omega]*[Above]
        Pmet=Matmul(Gamm,Temp)            ! [Gamma]-1*[Above]
        Prod=Pmet*Config%Wavelen          ! Above*Wavelength = Lambda [Gamma]^-1 [Omega] [Chi] [Phi]
        Pmet=Matmul(Prod,Config%UB)       ! [Above]*[D(UB)] = Lambda [Gamma]^-1 [Omega] [Chi] [Phi] [UB]

        Call Calpos(Dh,Pmet,Rnu,Dgamdom,Step,Dp0,Dgamma,Domega,Drnu)

        Gamma=Gamma+To_Deg*Dgamma        ! Add Changes
        Omega=Omega+To_Deg*Domega
        Rnu=Rnu+To_Deg*Drnu
        Xcalc=Xcalc+Dp0(1)               ! X <=> Nu <=> Anodes
        Ycalc=Ycalc+Dp0(2)               ! Y <=> -Gamma <=> Cathodes
        Zcalc=Zcalc+Dp0(3)               ! Z <=> Omega <=> Frames
        Prod(1,:)=Prod(1,:)-Dgamdom*Prod(2,:)   ! Modify Prod for Gam movement for later use.

        !!!   Set Up Indices Of Closest Reflections.
        !!!   Set up the default first.

        Dh1=0.0
        Dh2=0.0
        Dh3=0.0
        Dh1(1)=1.0
        Dh2(2)=1.0
        Dh3(3)=1.0

        !!!   Override By User-Given Values.
        If (Config%N_Neigh >= 4) Then
            Do I=1,3
                Dh1(I)=Config%Neigh(I,1)
                Dh2(I)=Config%Neigh(I,2)
                Dh3(I)=Config%Neigh(I,3)
                Dh4(I)=Config%Neigh(I,4)

                If (Config%N_Neigh >= 5) Then
                    Dh5(I)=Config%Neigh(I,5)
                    If (Config%N_Neigh >= 6) Dh6(I)=Config%Neigh(I,6)
                End If
            End Do
        End If

        Call Calpos(Dh1,Pmet,Rnu,Dgamdom,Step,Dp1,Dgamma,Domega,Drnu)
        Call Calpos(Dh2,Pmet,Rnu,Dgamdom,Step,Dp2,Dgamma,Domega,Drnu)
        Call Calpos(Dh3,Pmet,Rnu,Dgamdom,Step,Dp3,Dgamma,Domega,Drnu)

        Dp1Sq=Dp1(1)*Dp1(1)+Dp1(2)*Dp1(2)+Dp1(3)*Dp1(3)
        Dp2Sq=Dp2(1)*Dp2(1)+Dp2(2)*Dp2(2)+Dp2(3)*Dp2(3)
        Dp3Sq=Dp3(1)*Dp3(1)+Dp3(2)*Dp3(2)+Dp3(3)*Dp3(3)

        If (Config%N_Neigh >= 4) Then

            Call Calpos(Dh4,Pmet,Rnu,Dgamdom,Step,Dp4,Dgamma,Domega,Drnu)

            Dp4Sq=Dp4(1)*Dp4(1)+Dp4(2)*Dp4(2)+Dp4(3)*Dp4(3)

            If (Config%N_Neigh >= 5) Then
                Call Calpos(Dh5,Pmet,Rnu,Dgamdom,Step,Dp5,Dgamma,Domega, Drnu)
                Dp5Sq=Dp5(1)*Dp5(1)+Dp5(2)*Dp5(2)+Dp5(3)*Dp5(3)

                If (Config%N_Neigh >= 6) Then
                    Call Calpos(Dh6,Pmet,Rnu,Dgamdom,Step,Dp6,Dgamma,Domega, Drnu)
                    Dp6Sq=Dp6(1)*Dp6(1)+Dp6(2)*Dp6(2)+Dp6(3)*Dp6(3)
                End If
            End If
        End If

       !!!-------------------------------------------------------------------
       !!!      Find closest hkl of Twin, if necessary
       !!!-------------------------------------------------------------------
        Ncol = 35                           ! Lun for untwinned reflections
        Itype = 1                           ! untwinned reflection

        If (Config%Twin_UB_Set) Then
            Dht = Matmul(Twtmat,Hproc)          ! [Twin Ub]-1.[Proc Ub].[H]
            Dht = Real(Nint(Dht))               ! Get nearest integer indices
            vTemp=Matmul(Config%Twin_UB, Dht)   ! [Twin Ub].[Twin H]
            Rht =Matmul(Procel,vTemp)           ! [Ub]-1.[Twin Ub].[Twin H]
            Dh = Rht - Hproc

            Call Calpos(Dh,Pmet,Rnu,Dgamdom,Step,Dpt,Dgamma,Domega,Drnu)

            Dptsq=Dpt(1)*Dpt(1)+Dpt(2)*Dpt(2)+Dpt(3)*Dpt(3)

            If (Dptsq < 0.5) Then
                Itype = 3
                Ncol = 39
                Atype = 'Superposed'
            Else If (Dptsq < 100.0) Then
                Itype = 2
                Ncol = 39
                Atype = 'Twinned   '
            Else
                Atype = 'Resolved  '
            End If
        End If

        !!!-------------------------------------------------------------------
        !!!       Write data to ans.dat file, numor being processed to tty
        !!!-------------------------------------------------------------------
        !!!       Write(6,"(I10)") Numor
        Stvol=Config%ActVol*Step/Stang

        Write(I_Ans,40) Numors(Numor_Ind), (Hproc(I),I=1,3), Temper,  &
              Stang, Config%ActVol, Stvol, Config%VolFac, Config%SigBR, Config%IAddLib,  &
              Chihead, Phihead, Gammahead, Omegahead, Chi, Phi, Gamma, Omega, Rnu, Xcalc, Ycalc, Zcalc
        !start debug
        Write(I_debug,40) Numors(Numor_Ind), (Hproc(I),I=1,3), Temper,  &
              Stang, Config%ActVol, Stvol, Config%VolFac, Config%SigBR, Config%IAddLib,  &
              Chihead, Phihead, Gammahead, Omegahead, Chi, Phi, Gamma, Omega, Rnu, Xcalc, Ycalc, Zcalc
        !end debug

        40 Format(/,/,/,1X,'Numor=',I7,' (',3F6.2,')',20X,'Temp=',F6.1,  &
          ' K',/,1X,' Stang=',F5.2,' ActVol=',F7.1,' Stvol=',F7.1,  &
          ' VolFac=',F4.1,' SigBR=',F5.2,' IAddLib=',I1,/,  &
          1X,'Scan Centre at:        Chi    Phi  2Theta  Omega',/,21X,  &
          4F7.2,/,1X,'Calculated Centre at:  Chi    Phi   Gamma  Omega',  &
          '    Nu     Vert  Horiz  Frame',/,21X,5F7.2,1X,3F7.2)

        Write(I_Ans,74) Dh1, Dh2, Dh3, Dp1,Dp2,Dp3
        74   Format(' Vectors to Neighbouring Reflections :',/,  &
          3(' (',3F7.2,')'),/,3('  ',3F7.2,' '))

        If (Config%N_Neigh >= 4) Write(I_Ans,77) Dh4, Dh5, Dh6, Dp4,Dp5,Dp6
        77   Format(3(' (',3F7.2,')'),/,3('  ',3F7.2,' '))

        If (Config%Twin_UB_Set) Write(I_Ans,76) Dht,Atype,Dpt
        76   Format(' Vector to closest Twin Reflection   :',  &
          (10X,' (',3F7.2,')'),/,' Treat as ',A10,28X,('  ',3F7.2,' '))

        Ffrac=0.0                ! Initialise Filling Fraction
        Irange=Config%Imax-Config%Imin+1              !
        Ihi(1)=Irange                   !
        Jrange=Config%Jmax-Config%Jmin+1              !
        Ihi(2)=Jrange                   ! <= Determine Range Of Data
        Krange=Config%Kmax-Config%Kmin+1              !
        Ihi(3)=Krange                   !
        Kcen=Nint((1+Krange)*0.5)       ! Central Frame

        If (Config%Scale == 1) Then
            Smon = Real(Config%Monitor)/Ztim(Kcen)        ! Time Scaling Factor
        Else                                              ! Or
            Smon = Real(Config%Monitor)/Zmon(Kcen)        ! Monitor Scaling Factor
        End If

        Ctmax=0.0

        !!!-------------------------------------------------------------------
        !!!       Check for close neighbouring reflections and twin reflections
        !!!-------------------------------------------------------------------

        Do K=1,Krange
            Xp3=K+Config%Kmin-1-Zcalc
            Do J=1,Jrange
                Xp2=J+Config%Jmin-1-Ycalc
                Do I=1,Irange
                    Xp1=I+Config%Imin-1-Xcalc
                    ! Write(*,*)Xp1,Xp2,Xp3
                    ! Check For Close Neighbouring Reflections
                    If (Abs(Xp1*Dp1(1)+Xp2*Dp1(2)+Xp3*Dp1(3)) > 0.5*Dp1Sq) Then
                        Map(I,J,K)=4
                    !       Write(*,*)' Exceeds Dp1'
                    Else If (Abs(Xp1*Dp2(1)+Xp2*Dp2(2)+Xp3*Dp2(3)) > 0.5*Dp2Sq) Then
                        Map(I,J,K)=4
                    !       Write(*,*)' Exceeds Dp2'
                    Else If (Abs(Xp1*Dp3(1)+Xp2*Dp3(2)+Xp3*Dp3(3)) > 0.5*Dp3Sq) Then
                        Map(I,J,K)=4
                    !       Write(*,*)' Exceeds Dp3'
                    Else If (Config%N_Neigh >= 4 .And. Abs(Xp1*Dp4(1)+Xp2*Dp4(2)+Xp3*Dp4(3)) > 0.5*Dp4Sq) Then
                        Map(I,J,K)=4
                    !       Write(*,*)' Exceeds Dp4'
                    Else If (Config%N_Neigh >= 4 .And. Abs(Xp1*Dp5(1)+Xp2*Dp5(2)+Xp3*Dp5(3)) > 0.5*Dp5Sq) Then
                        Map(I,J,K)=4
                    !       Write(*,*)' Exceeds Dp5'
                    Else If (Config%N_Neigh >= 4 .And. Abs(Xp1*Dp6(1)+Xp2*Dp6(2)+Xp3*Dp6(3)) > 0.5*Dp6Sq) Then
                        Map(I,J,K)=4
                    !       Write(*,*)' Exceeds Dp6'
                    ! And For Twin Reflections
                    Else If (Config%Twin_UB_Set .And. &
                             Dptsq > 100.0 .And. &
                             Abs(Xp1*Dpt(1)+Xp2*Dpt(2)+Xp3*Dpt(3)) > 0.5*Dptsq) Then
                        Map(I,J,K)=4
                    !        Write(*,*)' Exceeds Dpt', I,J,K
                    Else
                        Map(I,J,K)=0
                    End If
                End Do
            End Do
        End Do

        !!!-------------------------------------------------------------------
        !!!       Average the background in 2Theta, to eliminate possible
        !!!       powder lines.
        !!!-------------------------------------------------------------------

        Call Powder(Map,Gamma,Rnu,Xcalc,Ycalc,Zcalc,Dgamdom, Step)

        !!!-------------------------------------------------------------------
        !!!       Prepare for the first analysis, of the projection onto Omega
        !!!-------------------------------------------------------------------

        Kfmin=Krange
        Kfmax=1

        Do K=1,Krange
            Ftot(K)=0.0                      ! Initialise Frame Totals
            Nftot(K)=0.0

            If (Config%Scale == 1) Then
                Rmon(K)=Ztim(Kcen)/Ztim(K)    ! Time scaling to scan centre
            Else
                Rmon(K)=Zmon(Kcen)/Zmon(K)    ! Monitor scaling to scan centre
            End If

            Do J=1,Jrange
                Do I=1,Irange
                    Vval(I,J,K)=Vval(I,J,K)*Rmon(K)   ! Data scaled for monitor or time
                    If (Map(I,J,K) == 0) Then
                        Ct=Vval(I,J,K)

                        If (Ct > Ctmax) Then       ! Search for highest value
                            Ctmax=Ct
                        End If

                        Ftot(K)=Ftot(K)+Ct            ! Collapse to omega scan
                        Nftot(K)=Nftot(K)+1
                    End If
                End Do
            End Do

            If (Nftot(K) < 20 .And. Ftot(K) == 0.0) Nftot(K)=0 !Avoid corners

            If (Nftot(K) /= 0) Then         ! Normalise the omega points
                Kfmin=Min(K,Kfmin)          ! and find the limits of unmasked points
                Ftot(K)=Ftot(K)*Irange*Jrange/Nftot(K)
            Else If (Kfmin /= Krange .And. Kfmax == 1) Then
                Kfmax=K-1
            End If
            !  Write(*,*)' Rmon, Ftot = ',K,Rmon(K),Ftot(K)
        End Do

        If (Kfmin == Krange) Kfmin=1

        If (Kfmax == 1)      Kfmax=Krange

        Kftot=Kfmax-Kfmin+1

        Write(I_Ans,"(1X,A,F5.2,A,I5)") 'Omega Scan with Step=',Step,' Deg, Npts:',Kftot

        Write(I_Ans,"(1X,15I5) ") (Nint(Ftot(K)),K=1,Krange)

        Scantot=0                       ! Calculate Scan Total

        Do K=Kfmin,Kfmax
            Scantot=Scantot+Ftot(K)
        End Do
        !!!---------------------------------------------------------------------
        !!!        Background iteration on omega
        !!!---------------------------------------------------------------------
        Newppts=0
        Bg=Scantot/Kftot

        Do  !80
            Nppts=Newppts
            Newppts=0
            Pksum=0

            Do K=Kfmin,Kfmax
                Pkthresh=Bg+2*Sqrt(Bg*Rmon(K))

                If (Ftot(K) > Pkthresh) Then                ! Peak points more
                    Pksum=Pksum+Ftot(K)                     ! than 2 Sigma from
                    Newppts=Newppts+1                       ! "Background".
                End If
            End Do

            Nbgpts=Kftot-Newppts
            If (Nbgpts > 0) Then
                Bg = Max(0.0,(Scantot-Pksum)/Nbgpts)
            Else
                Bg=0.0
            End If

            If (Newppts > Nppts .And. Nbgpts > 0) Cycle ! Do till convergence.

            Exit

        End Do

        Peak=0.0
        Perr=0.0
        Do K=Kfmin,Kfmax-1                         ! Integrated Intensity
            Dz=(Zomega(K+1)-Zomega(K))
            Peak=Peak+(Ftot(K)+Ftot(K+1)-2.0*Bg)*Dz   ! *Actual Steps
        End Do

        Peak=0.5*Peak/Step                 ! Trapezium Rule...
        !!! With step normalisation.
        Bgpp=Bg/(Jrange*Irange)            ! Background per pixel
        Appbgpp=Bgpp                       ! Keep approx Bgpp
        Perr = Sqrt(Max(Peak,0.0)+Max(Nppts,1)*(1+(Nppts/Nbgpts))*Bg)

        If (Nbgpts*Bg > 0.0) Then
            Bgpperr=Bgpp/Sqrt(Nbgpts*Bg)
        Else
            Bgpperr=0.0
        End If

        Write(I_Ans,"(1X,A,F7.1,A,F6.2,A,I2,A,/,1X,A,I7,A,I5,A,14X)")      &
         'Maximum Count Of',Ctmax-Bgpp,' Approximate Background Of',Bgpp,'(',Nint(100*Bgpperr), &
         ') Per Pixel.','Approximate Integrated Intensity =',Nint(Peak),'(',Nint(Perr),')'
        !start debug
        Write(I_debug,"(1X,A,F7.1,A,F6.2,A,I2,A,/,1X,A,I7,A,I5,A,14X)")      &
         'Maximum Count Of',Ctmax-Bgpp,' Approximate Background Of',Bgpp,'(',Nint(100*Bgpperr), &
         ') Per Pixel.','Approximate Integrated Intensity =',Nint(Peak),'(',Nint(Perr),')'
        !end debug

        Sigtonoise=Peak/Max(1.0,(Bg*(Kftot-Nbgpts))) ! Signal To Noise

        If (Sigtonoise > Config%SigBR) Then
            Jflag(7)=1                           ! Set Strong Flag
        End If

        Peakapp=Peak                             ! Retain Approx. Peak
        !!! For Later Check.
        Continue

        If (Xcalc < Real(Config%Imin)+2) Jflag(17)=1
        If (Xcalc > Real(Config%Imax)-2) Jflag(17)=1
        If (Ycalc < Real(Config%Jmin)+2) Jflag(17)=1
        If (Ycalc > Real(Config%Jmax)-2) Jflag(17)=1
        If (Zcalc < Real(Config%Kmin)+5) Jflag(17)=1
        If (Zcalc > Real(Config%Kmax)-5) Jflag(17)=1

        If (Jflag(17) == 1) Then        ! Centre Outside Data, reflection at borders
            Write( *,"(/,1X,A)")'**** The Calculated Centre is too close to the Data Array Boundaries! ****'
            Write(I_Ans,"(/,1X,A)")'**** The Calculated Centre is too close to the Data Array Boundaries! ****'
            !   Call Flag(Jflag,Numor)
            Cycle do_Ref                        ! Get Next Reflection.
        End If

        If (Jflag(7) == 1) Goto 295     ! Jump if Strong

      !------------------------------------------------------------------
      !       Select library model for weak peak processing
      !------------------------------------------------------------------
     55 Continue

        If (Config%IAddLib == 1) Cycle do_Ref                 ! Ignore Weak Peaks

        If (Jflag(7) == 1) Jflag(8)=1

        If (Itype == 2) Then
            Write(I_Ans,"(A,3F6.2)") ' Weak Twinned Reflection. Peak Assumed (As Above) At ...', &
                              Xcalc,Ycalc,Zcalc         ! Weak Twinned Reflection

            Call Results(Numors(Numor_Ind), Hproc, Peak, Perr, Rnu, Gamma, Temper, Ncol, Config%ICard, &
                         Omega, Chi, Phi, Step, Stang, Smon, Jflag, Machine)
            Cycle do_Ref                         ! Get Next Reflection.
        End If

        If (Nlib <= 0) Then
            Write(I_Ans,"(A,3F6.2)") ' There Are No Library Models. Peak Assumed (As Above) At ...', &
                              Xcalc,Ycalc,Zcalc         ! Empty Library.
            Call Results(Numors(Numor_Ind), Hproc, Peak, Perr, Rnu, Gamma, Temper, Ncol, Config%ICard, &
                         Omega, Chi, Phi, Step, Stang, Smon, Jflag, Machine)
            Cycle do_Ref                      ! Get Next Reflection.
        End If

        If (Gammahead > Pklib(1,2)) Then                 ! If Gammahead beyond
            If (Gammahead < Pklib(Nlib,2)) Then          ! Gamma within library,
                Do I=2,Nlib                              ! search library for
                    If (Gammahead >= Pklib(I,2)) Cycle   ! bracketing models and
                    Do N=5,12                            ! use weighted mean.
                        Pk(N)=(Gammahead-Pklib(I-1,2))*Pklib(I,N)
                        Pk(N)=Pk(N)+(Pklib(I,2)-Gammahead)*Pklib(I-1,N)
                        Pk(N)=Pk(N)/(Pklib(I,2)-Pklib(I-1,2))
                    End Do

                    Pk(13)=((Gammahead-Pklib(I-1,2))*Pklib(I,13))**2
                    Pk(13)=Pk(13)+((Pklib(I,2)-Gammahead)*Pklib(I,13))**2
                    Pk(13)=Sqrt(Pk(13))/(Pklib(I,2)-Pklib(I-1,2))
                    Write(I_Ans,"(A,F5.2,A,2I6)") ' Weak Peak. Peak/Bg In Omega Profile = ',Sigtonoise, &
                                         ' Numors Used As Models',Nint(Pklib(I-1,1)),Nint(Pklib(I,1))
                    Exit
                End Do
            Else
                Do N=5,13                           ! Otherwise
                    Pk(N)=Pklib(Nlib,N)             ! use last model.
                End Do

                Write(I_Ans,"(A,F5.2,A,I6)") ' Weak Peak. Peak/Bg in Omega Profile = ',Sigtonoise, &
                                     ' Numor Used as Model',  Nint(Pklib(Nlib,1))
            End If
        Else
            Do N=5,13                             ! First model, continue,
                Pk(N)=Pklib(1,N)                  ! Otherwise use first model.
            End Do
            Write(I_Ans,"(A,F5.2,A,I6)") &
            ' Weak Peak. Peak/Bg in Omega Profile = ',Sigtonoise,' Numor used as Model',Nint(Pklib(1,1))
        End If

        !740                            ! Load Library
        Elplib(1,1)=Pk(5)               ! Ellipsoid Into
        Elplib(2,2)=Pk(6)               ! Elplib
        Elplib(3,3)=Pk(7)
        Elplib(1,2)=Pk(8)
        Elplib(2,1)=Elplib(1,2)
        Elplib(1,3)=Pk(9)
        Elplib(3,1)=Elplib(1,3)
        Elplib(2,3)=Pk(10)
        Elplib(3,2)=Elplib(2,3)
        Frac=Pk(11)
        Stvol=Pk(12)
        Fracerr=Pk(13)

        Scal =0.0               !   Set Up Transformation Matrices To
        Shear =0.0              !   To 'Shear' Ellipsoids For Gamma-
                              ! <= Coupled Scans And Return
        Scal(1,1)=1.0           !   From "Standard" Coordinates
        Scal(2,2)=1.0           !   To Coordinates For This Reflection.
        Scal(3,3)=Step/Stang
        Shear(1,1)=1.0
        Shear(2,2)=1.0
        Shear(3,3)=1.0
        Sh=-Dgamdom*Step*To_Rad*Config%DRad/Dy       ! Shearing Coefficient
        Shear(2,3)=Sh
        Temp  =Matmul(Elplib,Scal)                   ! Restore to measurement
        Elplib=Matmul(Scal,  Temp)                   ! coordinates
        Temp  =Matmul(Elplib,Shear)
        Shear(2,3)=0.0
        Shear(3,2)=Sh
        Elps = Matmul(Shear,Temp)                    ! Shear  Ellipsoid
        Temp=Elps

        Call Diagonalize_Sh(Temp,3,Valu,Dircos)      ! Find Eigenvalues

        Do I=1,3
            Valu(I)=Sqrt(1.0/Valu(I))                ! Principal Axes
        End Do

        Elpvol=4.0*Pi*Valu(1)*Valu(2)*Valu(3)/3.0    ! Ellipse Volume

        Write(I_Ans,"(A,30X,A,3F7.3)") '   With Principal Axes','Vert',(Dircos(1,L),L=1,3)
        Write(I_Ans,"(1X,3F7.2,A,3F7.3)") (Valu(N),N=1,3),' ... Directions Cosines..=>  Horiz',(Dircos(2,L),L=1,3)
        Write(I_Ans,"(A,I5,A,F6.3,A,3F7.3)") ' Volume',Nint(Elpvol),' Elements, Containing', &
                                             Frac,' Of Peak   Frames',(Dircos(3,L),L=1,3)
        Cg(1)=Xcalc-Config%Imin+1
        Cg(2)=Ycalc-Config%Jmin+1                    ! Set C. Of G. To
        Cg(3)=Zcalc-Config%Kmin+1                    ! Calculated Value.
        write(unit=I_Ell,fmt=*) Numors(Numor_Ind), H, Cg, Valu, Dircos, ElpVol
        Goto 290
        !!!------------------------------------------------------------------
        !!!       Or get FWHH of Strong Reflection
        !!!------------------------------------------------------------------
    295 Continue

        Ftotmax=0.0
        Do Ifw=Kfmin,Kftot                           ! Find highest frame total
            If (Ftot(Ifw) > (Ftotmax)) Ftotmax=Ftot(Ifw)
        End Do

        Fht=0.5*(Ftotmax-Bg)                         ! Half Height

        Do Ifw=Kfmin,Kfmax
            If (Ftot(Ifw) > (Fht+Bg)) Exit           ! Over Half Height
        End Do

        Ikmin=Ifw                                    ! First Point

        Do Ifw=Kfmax,Ikmin,-1
            If (Ftot(Ifw) > (Fht+Bg)) Exit
        End Do

        Ikmax=Ifw                                    ! Last Point
        Fwhh = Abs(Zomega(Ikmax)-Zomega(Ikmin))+Step
        Thresh=Bgpp+Config%Cont*(Ctmax-Bgpp)         ! Threshold for contour
        !!!------------------------------------------------------------------
        !!!       ..... And Now Find Contour
        !!!------------------------------------------------------------------
        Mass=0                           ! Total points in contour
        Cg(:)=0.0                        ! Initialise C. Of G.
        Tens(:,:)=0.0                    ! Initialise Inertia Tensor

        Do K=1,Krange                    ! Revert to krange since Map used
            Xpos(3)=Real(K)              !
            Do J=1,Jrange
                Xpos(2)=Real(J)                !
                Do I=1,Irange
                    Xpos(1)=Real(I)          ! Load element to Xpos(N)
                    ! Acceptable point above Contour level
                    If (Map(I,J,K) /= 4 .And. Vval(I,J,K) > Thresh) Then
                        Mass=Mass+1
                        Do N=1,3
                            Cg(N)=Cg(N)+Xpos(N)    ! Form C. Of G. Sum
                            Do M=1,3
                                Tens(M,N)=Tens(M,N)+Xpos(M)*Xpos(N)     ! Inertia Tensor
                            End Do
                        End Do
                    End If
                End Do
            End Do
        End Do

        If (Mass < 10) Goto 55        ! Temporary Fix, Until Est. Fixed, GJM

        Do N=1,3
            Cg(N)=Cg(N)/Real(Mass)         ! Normalise to unit weight
            Do M=1,3
                Tens(M,N)=Tens(M,N)/Mass
            End Do
        End Do

        Xobs=Cg(1)+Real(Config%Imin-1)
        Yobs=Cg(2)+Real(Config%Jmin-1)
        Zobs=Cg(3)+Real(Config%Kmin-1)

        Write(I_Ans,"(a,f5.2)")             ' Strong Peak..Omega Full Width at Half Height',Fwhh,' Deg'
        Write(I_Ans,"(a,f5.2,a,i4,a,3f6.2)")' Observed Centre of Gravity of',Config%Cont,' Contour (',  &
                                               Mass,' Points) at',Xobs,Yobs,Zobs
        !Start debug
        Write(I_debug,"(a,f5.2)")             ' Strong Peak..Omega Full Width at Half Height',Fwhh,' Deg'
        Write(I_debug,"(a,f5.2,a,i4,a,3f6.2)")' Observed Centre of Gravity of',Config%Cont,' Contour (',  &
                                                Mass,' Points) at',Xobs,Yobs,Zobs
        !End debug
        !!!------------------------------------------------------------------
        !!!       Trap Reflections Too Close To Scan Boundaries!
        !!!------------------------------------------------------------------
        If (Xobs < Real(Config%Imin)+2) Jflag(17)=1
        If (Xobs > Real(Config%Imax)-2) Jflag(17)=1
        If (Yobs < Real(Config%Jmin)+2) Jflag(17)=1
        If (Yobs > Real(Config%Jmax)-2) Jflag(17)=1
        If (Zobs < Real(Config%Kmin)+4) Jflag(17)=1
        If (Zobs > Real(Config%Kmax)-4) Jflag(17)=1

        If (Jflag(17) == 1) Then        ! Centre Outside Data
            Write(I_Ans,"(1X,A,/)") '*** The Observed Centre is too close to the Data Array Boundaries!'
            Call Flag(Jflag, Numors(Numor_Ind))
            Cycle do_Ref                        ! Get Next Reflection.
        End If

        Trace=Tens(1,1)+Tens(2,2)+Tens(3,3)
        Rsq=dot_product(Cg,Cg)

        Do N=1,3                                !
            Do M=1,3                              !
                Tens(M,N)=-Tens(M,N)+Cg(M)*Cg(N)    ! <= Parallel Axis Theorem
            End Do                                !
            Tens(N,N)=Tens(N,N)+Trace-Rsq       !
        End Do

        Call Diagonalize_Sh(Tens,3,Valu,Dircos)   ! Find eigenvalues,
                                                  ! and directions cosines
        Trace=Valu(1)+Valu(2)+Valu(3)

        Do N=1,3
            Valu(N)=(2.5*Trace-5.0*Valu(N))       ! Store in Valu(N)
            If (Valu(N) <= 0.0) Then
                Write(I_Ans,"(A,/,A)") '  The Data Points give a Non-Ellipsoidal Shape,',  &
                                       '  Reflection subsequently treated as Weak.'
                Jflag(9)=1                      ! Set Report Flag
                Goto 55                         ! Go to weak peak processing.
            End If
            Valu(N)=Sqrt(Valu(N))
        End Do

        Elpvol=4.0*Pi*Valu(1)*Valu(2)*Valu(3)/3.0 ! Ellipse Volume

        Write(I_Ans,"(A,30X,A,3F7.3)") '   With Principal Axes','Vert',(Dircos(1,L),L=1,3)
        Write(I_Ans,"(1X,3F7.2,A,3F7.3)") (Valu(N),N=1,3),' ... Directions Cosines..=>  Horiz',(Dircos(2,L),L=1,3)
        Write(I_Ans,"(A,I7,A,22X,A,3F7.3)") ' And Volume',Nint(Elpvol),' Elements.','Frames',(Dircos(3,L),L=1,3)
        !!!------------------------------------------------------------------
        !!!       Trap Flat Ellipsoids!
        !!!------------------------------------------------------------------
        If (Elpvol < 1.0) Then
            Jflag(10) = 1                             ! 'Flat' Flag
            Write(I_Ans,"(1X,A)") '**** Flat Ellipsoid **** '

            Call Results(Numors(Numor_Ind), Hproc, Peak, Perr, Rnu, Gamma, Temper, Ncol, Config%ICard, &
                         Omega, Chi, Phi, Step, Stang, Smon, Jflag, Machine)

            Cycle do_Ref                               ! Get Next Reflection
        End If
        !!!-------------------------------------------------------------------
        Do I=1,3
            Do J=1,3
                Prin(I,J)=0
                Socrid(I,J)=Dircos(J,I)               ! Transpose Direction
            End Do                                    ! Cosines Matrix
            Prin(I,I)=1/Valu(I)**2
        End Do

        Temp=Matmul(Dircos,Prin)
        Elps=Matmul(Temp,Socrid)
        Ratio=(Elpvol*Step)/(Stvol*Stang)
        !Start debug
             Call Display_IJ(i_debug)
        !End debug
        !!!----------------------------------------------------------------------
        !!!       Optional Display of Modelling Contour
        !!!----------------------------------------------------------------------
        If (Config%IPrint == 2) Then
            Vol(1)=1.0                       ! Model Contour Volume
            Vol(2)=1.0/Ratio                 ! "Core" Volume
            Vol(3)=Config%VolFac*Vol(2)      ! Peak Volume

            Call Elplim(Elps,Ihi,Cg,Vol(3),Maxx,Minn,Jflag)
            Call Plan(Elps,Cg,Vol,Map,Maxx,Minn,Mask,Jflag)
            Write(I_Ans,"(/,A,/,A,F5.2,A)") ' Modelling Volumes',' Modelling Contour (',Config%Cont, &
                   ' Peak Height) = 1 Core Volume = 2  Peak Volume = 3  Background Etc = 0 '
            Call Display(Minn,Maxx,Map)
        End If
        !!!---------------------------------------------------------------------
        Ratio=(Ratio)**(2.0/3.0)
        Elps(:,:)=Elps(:,:)*Ratio               ! Prepare "Corer" Ellipsoid
        Ffrac=Mass/Elpvol                       ! warn of only partial
        If (Ffrac < Config%Fill) Then           ! ellipsoid filling
            Write(I_Ans,"(A,F5.2,A,/,A)") ' ***Warning... Filling Fraction Only ',Ffrac, &
                                    '. Modelling Contour Non-Ellipsoidal.',        &
                                    ' This Peak will not be added to the Library, but treated as Weak***'

            Jflag(9)=1                          ! Set Report Flag
            Goto 55                             ! Go To Weak Peak
        End If                                  ! Processing.
        !!!-
        Izobs=Int(Zobs)                         ! Interpolate Observed
        Frptz=Zobs-Izobs                        ! angles For Rafin
        !write(*,*) Zobs,Izobs,Frptz,Zgamma(Izobs)
        Gamobs= Zgamma(Izobs)*(1.0-Frptz)+Zgamma(Izobs+1)*Frptz
        Omobs = Zomega(Izobs)*(1.0-Frptz)+Zomega(Izobs+1)*Frptz

        If (Machine == 0) Then
            Anglev=Chi
        Else
            Anglev=Rnum
        End If

        !!!---- Omega:Xtheta Scan
        If (Snum%Manip == 2) Write(Rafbuf,"(9F8.2)") (Hproc(I),I=1,3),Xobs*Dx,  &
          Yobs*Dy,Gamobs, Omobs,Anglev, Phi
        !!!---- Phi Scan (Logic To Be Checked 15/11/96)
        If (Snum%Manip == 4) Write(Rafbuf,"(9F8.2)") (Hproc(I),I=1,3),Xobs*Dx,  &
          Yobs*Dy, Gamobs, Omega,Anglev, Omobs   ! Write To Rafin Record Buffer

        !!!---- Include Yobs In Gamobs for later Lorentz Correction and output to
        !!!---- Coll5 File.  Gjmci 6/2/94. Is the sign before Yoffset correct?
!        Gamobs=Gamobs+ Atand((15.5 - Yobs)*Dy - Config%YOffset)/Config%DRad)
        Gamobs=Gamobs+ Atand(((0.5*real(n_ver-1) - Yobs)*Dy - Config%YOffset)/Config%DRad)

        !!!-----------------------------------------------------------------------
        !!!       Following Section Common To Strong And Weak Processing
        !!!-----------------------------------------------------------------------
    290 Continue

        Vol(1)=1.0                      ! Core Ellipsoid,
        Vol(2)=Config%VolFac            ! Peak    "
        Vol(3)=2.0*Vol(2)               ! Background " .

        Call Elplim(Elps,Ihi,Cg,Vol(3),Maxx,Minn,Jflag)
        Call Plan(Elps,Cg,Vol,Map,Maxx,Minn,Mask,Jflag)
        !!!----------------------------------------------------------------------
        !!!       Optional Display Of Integration Contour
        !!!----------------------------------------------------------------------
        If (Config%IPrint == 1) Then
            Write(I_Ans,"(/,A,/,A)") ' Integration Volumes.',&
                                 ' Core Volume = 1 Peak Volume = 2  Background Volume = 3   Etc = 0 '
            Call Display(Minn,Maxx,Map)
        End If
        !!!----------------------------------------------------------------------
        Suma(:)=0.0                  ! Totals Of Points
        Ntot(:)=0                    ! In Core And Peak

                                     ! Set Up Approx
                                     ! Background And Error
        Do I=Minn(1),Maxx(1)         ! In Case Not
            Do J=Minn(2),Maxx(2)     ! Available From
                Bgp(I,J)=Appbgpp     ! Backgr Routine.
                Bgerr(I,J)=Bgpperr
            End Do
        End Do

        Call Backgr(Minn,Maxx,Map,Bgp,Bgerr,Jflag)

        Bgpp=0.

        Do I=Minn(1),Maxx(1)
            Do J=Minn(2),Maxx(2)
                Bgpp=Bgpp+Bgp(I,J)
                Do N=1,2
                    Npb(N,I,J)=0
                End Do
            End Do
        End Do

        Bgpp=Bgpp/((Maxx(1)-Minn(1)+1)*(Maxx(2)-Minn(2)+1))

        Do K=Minn(3),Maxx(3)
            Cnt(K)=0.
            Cnterr(K)=0.
            Frame(K)=0.
            Do J=Minn(2),Maxx(2)
                Do I=Minn(1),Maxx(1)
                    If (Map(I,J,K) == 4) Cycle                ! Masked out point.
                    Frame(K)=Frame(K)+Vval(I,J,K)-Bgp(I,J)    ! All counts on frame K
                    If (Map(I,J,K) > 0) Then                  ! within Background Box
                        N=Map(I,J,K)
                        If (N /= 3) Then
                            Cnt(K)=Cnt(K)+Vval(I,J,K)-Bgp(I,J)   ! Peak counts on frame K
                            Cnterr(K)=Cnterr(K)+Vval(I,J,K)+Bgerr(I,J)**2
                            Suma(N)=Suma(N)+Vval(I,J,K)          ! Counts in core, Peak.
                            Npb(N,I,J)=Npb(N,I,J)+1
                            Ntot(N)=Ntot(N)+1                    ! Totals of points
                        End If
                    End If
                End Do
            End Do
        End Do

        Peak=Suma(1)
        Perr=Peak

        If (Jflag(11) /= 1) Then
            Do J=Minn(2),Maxx(2)
                Do I=Minn(1),Maxx(1)
                    Peak=Peak-Npb(1,I,J)*Bgp(I,J)
                    Perr=Perr+(Npb(1,I,J)*Bgerr(I,J))**2
                End Do
            End Do
        Else
            Peak=Suma(1)-Ntot(1)*Bgpp
            Perr=Perr+(Ntot(1)*Bgpperr)**2
        End If

        !!!------------------------------------------------------------------
        !!!       Trap large disagreements between approx. and ellipsoid
        !!!       intensity, unless reflection already Weak.
        !!!------------------------------------------------------------------
        If (Jflag(8) /= 1 .And. Abs(Peakapp/Peak) > 1.5) Then
            Write(I_Ans,"(A,I7,A,I5,A,/,A)") '  Ellipsoid-Integrated Intensity =',Nint(Peak),'(', &
                                            Nint(Perr),') < 2/3 Approximate Intensity',       &
                                         '  Reflection Subsequently Treated As Weak.'
            Jflag(8)=1                          ! Set report flag
            Goto 55                             ! go to weak peak
        End If                                  ! processing.
        !!!------------------------------------------------------------------

        Peak1=Peak
        Do K=Minn(3),Maxx(3)
            Cnterr(K)=Sqrt(Amax1(0.0,Cnterr(K)))
        End Do

        If (Config%IPrint == 3) Call Graph(Cnt,Cnterr,Frame,Maxx,Minn)! Plot
        !!!---------------------------------------------------------------------
        !!!       Results for weak peak when library has been used
        !!!---------------------------------------------------------------------
        If (Ffrac <= Config%Fill) Then     ! ..Else List Results As For Weak
            Call Elplim(Elps,Ihi,Cg,Vol(1),Maxx,Minn,Jflag)
                                                    ! Set Jflag On Core Vol
            Perr=Perr/(Peak**2)
            Perr=Perr+(Fracerr/Frac)**2             !  Add Term For Df,
            Peak=Peak/Frac                          ! Correct For F,Then
            Perr=Sqrt(Perr)*Abs(Peak)               ! Multiply By Intensity
            Write(I_Ans,"(A,I7,A,I4,A,F6.2,A,3F6.2)") ' Intensity=',Nint(Peak),'(',Nint(Perr),') With Mean Background', &
                                               Bgpp,' Assumed Centre', Xcalc,Ycalc,Zcalc

            Call Results(Numors(Numor_Ind), Hproc, Peak, Perr, Rnu, Gamma, Temper, Ncol, Config%ICard, &
                         Omega, Chi, Phi, Step, Stang, Smon, Jflag, Machine)
            !  Write Centre, Twin And Dp Values To *.Fly File          C.W. April 2000         (CW 16/4/00)
            !Write(I_Fly,"(F7.0,6F6.2,L1,I3)") Real(Numors(Numor_Ind)), Xcalc, Ycalc, Zcalc, Xobs, Yobs, Zobs, &
            !                                  Config%Twin_UB_Set, Config%N_Neigh
            Write(I_Fly,"(F7.0,6F6.2,I4)") Real(Numors(Numor_Ind)), Xcalc, Ycalc, Zcalc, Xobs, Yobs, Zobs, &
                                               Config%N_Neigh
            Write(I_Fly,"(15F6.1)") Dp1, Dp2, Dp3, Dp4, Dpt

            Cycle do_Ref                               ! Get Next Reflection
        End If
        !!!---------------------------------------------------------------------
        !!!       Strong Peak Output
        !!!---------------------------------------------------------------------

        !If (Itype /= 2) Write(I_Raf,'(A80)') Rafbuf         ! Write To Rafin
        Write(I_Raf,'(A80)') Rafbuf         ! Write To Rafin

        Call Elplim(Elps,Ihi,Cg,Vol(2),Maxx,Minn,Jflag)
                                               ! Set Jflag On Peak Vol
        Peak=Suma(1)+Suma(2)
        Perr=Peak

        If (Jflag(11) /= 1) Then
            Do I=Minn(1),Maxx(1)
                Do J=Minn(2),Maxx(2)
                    Peak=Peak-(Npb(1,I,J)+Npb(2,I,J))*Bgp(I,J)
                    Perr=Perr+((Npb(1,I,J)+Npb(2,I,J))*Bgerr(I,J))**2
                End Do
            End Do
        Else
            Peak=Peak-(Ntot(1)+Ntot(2))*Bgpp
            Perr=Perr+((Ntot(1)+Ntot(2))*Bgpperr)**2
        End If

        Perr=Sqrt(Perr)                         ! Peak  Error

        If (Peak == 0.0) Then
            Write(*,*)' Peak = 0.0'
            call finish()
        End If

        Frac=Peak1/Peak
        Dfds(1)=(1-Frac)/Peak                   !
        Dfds(2)=(Frac/Peak)                     ! <= D(Frac)/D(Sum(N)
        Fracerr=0.0

        Do I=1,2
            Fracerr=Fracerr+Suma(I)*(Dfds(I)**2)
        End Do

        Fracerr=Sqrt(Fracerr)                   ! Uncertainty In Frac

        Write(I_Ans,"(A,I7,A,I4,A,F6.2,/,A,F6.3,A,I2,A )") ' Integrated Intensity ',Nint(Peak),        &
                                                      '(',Nint(Perr),') Mean Background',Bgpp,    &
                                                      ' "Standard" Core Contains ', Frac,'(',     &
                                                       Nint(Fracerr*100.),') Of Peak Intensity'
        !start debug
        Write(I_debug,"(A,I7,A,I4,A,F6.2,/,A,F6.3,A,I2,A )") ' Integrated Intensity ',Nint(Peak),        &
                                                      '(',Nint(Perr),') Mean Background',Bgpp,    &
                                                      ' "Standard" Core Contains ', Frac,'(',     &
                                                       Nint(Fracerr*100.),') Of Peak Intensity'
        !end debug

        Delang(1)=-(Yobs-Ycalc)*Dy/(Config%DRad*Cosd(Rnu))
        Delang(2)=To_Rad*(Zobs-Zcalc)*Step                ! Differences in Obs
        Delang(3)=(Xobs-Xcalc)*Dx/Config%DRad             ! and Calc angles.
        Diff=To_Deg*Sqrt(Delang(1)**2+Delang(2)**2+Delang(3)**2)

        If (Diff > Config%DiffLim) Jflag(12)=1            ! Report on big diff

        Call Results(Numors(Numor_Ind), Hproc, Peak, Perr, Rnu, Gamma, Temper, Ncol, Config%ICard, &
                     Omega, Chi, Phi, Step, Stang, Smon, Jflag, Machine)

        !!!       Write Centre, Twin And Dp Values To *.Fly File (Cw 16/4/00)
        !Write(I_Fly,"(F7.0,6F6.2,L1, I3)") Real(Numors(Numor_Ind)), Xcalc, Ycalc, Zcalc, Xobs, Yobs, Zobs, &
        !                                   Config%Twin_UB_Set, Config%N_Neigh
        Write(I_Fly,"(F7.0,6F6.2,I4)") Real(Numors(Numor_Ind)), Xcalc, Ycalc, Zcalc, Xobs, Yobs, Zobs, &
                                       Config%N_Neigh
        Write(I_Fly,"(15F6.1)") Dp1, Dp2, Dp3, Dp4, Dpt

        !!!-------------------------------------------------------------------
        !!!       Set Up Matrix Elements For Ub, Offset Refinement
        !!!-------------------------------------------------------------------
        Do Ii=1,3
            Do Jj=Ii,3
                Square(Ii,Jj)=0.0
                Do N=1,3
                    Square(Ii,Jj)=Square(Ii,Jj)+Prod(N,Ii)*Prod(N,Jj)
                End Do

                Do I=1,3
                    Do J=1,3
                        Iii=3*(Ii-1)+I
                        Jjj=3*(Jj-1)+J
                        Cne(Iii,Jjj)=Cne(Iii,Jjj)+Square(Ii,Jj)*Hproc(I)*Hproc(J)
                    End Do
                End Do
            End Do
        End Do

        Do Ii=1,3
            Do I=1,3
                Iii=3*(Ii-1)+I
                Do J=1,3
                    Jjj=9+J
                    Prh=Prod(J,Ii)*Hproc(I)
                    Cne(Iii,Jjj)=Cne(Iii,Jjj)+Prh
                    Dne(Iii)=Dne(Iii)+Prh*Delang(J)
                End Do
            End Do
        End Do

        Do J=1,3
            Cne(9+J,9+J)=Cne(9+J,9+J)+1.0
            Dne(9+J)=Dne(9+J)+Delang(J)
        End Do

        Do I=1,3
            Sumsq=Sumsq+Delang(I)**2
        End Do

        Iub=Iub+1
        !!!-------------------------------------------------------------------
        !!!       Library Storage
        !!!-------------------------------------------------------------------
        If (Nlib >= 1000) Then
            Write(I_Ans,"(A)") ' ****Warning...The Library Now Contains 1000 Peaks And Has Been Frozen! ****'
            Cycle do_Ref
        End If

        If (Itype == 2) Cycle do_Ref               ! Twinned Reflection

        If (Config%IAddLib == 0) Cycle do_Ref     ! Library Frozen

        Sh=Dgamdom*To_Rad*Step*Config%DRad/Dy

        Do N=1,3                          !     Set up transformation
            Do M=1,3                      !     matrices
                Scal(M,N)=0.0             !     for Dgamma/Domega
                Shear(M,N)=0.0            !    'Shear' and conversion
            End Do                        ! <=  From Detector to
        End Do                            !
        Shear(1,1)=1.0                    !    'Standard' Coordinates
        Shear(2,2)=1.0
        Shear(3,3)=1.0
        Shear(2,3)=Sh

        Scal(1,1)=1.0
        Scal(2,2)=1.0
        Scal(3,3)=Stang/Step

        Temp  = Matmul(Elps,Shear)           ! Transform to
        Shear(2,3)=0.0                       !'Stationary Detector'
        Shear(3,2)=Sh                        !

        Elps  = Matmul(Shear,Temp)           ! Make conversion
        Temp  = Matmul(Elps,Scal)            ! to 'Standard'
        Elplib= Matmul(Scal,Temp)            ! Coordinates.

        Pk(1)  = Real(Numors(Numor_Ind))
        Pk(2)  = Gammahead
        Pk(3)  = Phi
        Pk(4)  = Anglev
        Pk(5)  = Elplib(1,1)
        Pk(6)  = Elplib(2,2)
        Pk(7)  = Elplib(3,3)
        Pk(8)  = Elplib(1,2)
        Pk(9)  = Elplib(1,3)
        Pk(10) = Elplib(2,3)
        Pk(11) = Frac
        Pk(12) = Stvol
        Pk(13) = Fracerr

        If (Nlib <= 0) Then
            Call Libent(Pklib,Pk,1)                 ! Make first entry
            Nlib=1
            Cycle do_Ref                            ! Get next reflection
        End If

        If (Gammahead > Pklib(Nlib,2)) Then
            Nlib=Nlib+1
            Call Libent(Pklib,Pk,Nlib)              ! Make highest entry
            Cycle do_Ref                            ! Get next reflection
        End If

        Do N=1,Nlib
            If (Nint(Pklib(N,1)) /= Numors(Numor_Ind)) Cycle
            Call Libent(Pklib,Pk,N)              ! Overwrite entry
            Cycle do_Ref                         ! Get next reflection
        End Do

        Do N=1,Nlib                              ! Put in Gammahead
            If (Gammahead > Pklib(N,2)) Cycle    ! order.
            Do M=Nlib,N,-1
                Do L=1,13                        ! Make Room for entry
                    Pklib(M+1,L)=Pklib(M,L)
                End Do
            End Do

            Call Libent(Pklib,Pk,N)              ! Make normal entry

            Nlib=Nlib+1
            Cycle do_Ref                         ! Get next reflection
        End Do

   End Do do_Ref                                 ! Get next reflection

   ! Close the file that contains the Ellipsoid parameters
   close(unit=I_Ell)

   If (Config%IRefine < 0) Then                   ! UB Refinement at end (old goto 556)
       If (Iub > Max(12,Config%IRefine-1)) Then
           Do I=1,12
               Do J=1,I
                   Cne(I,J)=Cne(J,I)
               End Do
           End Do

           !Call Invert_Matrix(Cne,Enc,Singular)               ! UB Refinement every irefine
           singular=.false.
           Enc=Inverse_Matrix(Cne)
           if(err_CFML%Ierr /= 0) then
               singular=.true.
               write(*,"(a)") " => "//trim(err_CFML%Msg)
               call Clear_Error()
           end if

           If (.Not. Singular) Then                           ! (Or More!)
               Call Refine(Cne,Dne,Dpar,Machine)              ! Strong reflections
               Write(I_Ubm,"(F10.4,t65,a)") To_Deg*Sqrt(Sumsq/Iub),"!  Quality factor: To_Deg*Sqrt(Sumsq/Iub)"
               Write(I_Ubm,"(/t65,a/)")     "!  All numors have been used for UB-refinement"
               Iub=0                                          ! Reset strong peak count
               Sumsq=0.0                                      !   "  Sums ( Diff)**2
           End If
       else
           Write(I_Ubm,"(a,i2,a)") " *****  WARNING: the number of strong reflections is ",iub,", which is less than 12 !"
           Write(I_Ubm,"(a)")      " *****  NO REFINEMENT of the UB-matrix and offsets is possible!"
       End If
   End If

   Close(I_Lib)

   ! This File-Handling needs to be tidied up some more.
   If (.Not. Openf2(I_Lib, Cfg_Basename, '.lib', 'New')) Then
      If (.Not. Openf2 (I_Lib, Cfg_Basename, '.lib', 'Old')) call finish()
   End If

   If (Nlib > 0) Write(I_Lib,"(F7.0,3F9.3,6G10.3,3F9.3)") ((Pklib(I,J),J=1,13),I=1,Nlib) ! Write New Library.

   Call Cpu_Time(Time_End)

   Write(Unit=*,fmt="(a,f10.4,a)") " => Cpu-Time: ",Time_End-Time_Start," Seconds"

   If (Allocated(Numors))                  Deallocate(Numors)
   If (Allocated(Config%Numors_Intervals)) Deallocate(Config%Numors_Intervals)
   If (Allocated(Snum%Tmc_Ang))            Deallocate(Snum%Tmc_Ang)
   If (Allocated(Snum%Counts))             Deallocate(Snum%Counts)
   close(unit=i_debug)
   call finish()

   contains

    subroutine finish()
      Write(*,"(/,a)") " => Press <enter> to finish "
      Read(*,'(a)') filin
      stop
    end subroutine finish

 End Program Racer_CFML
