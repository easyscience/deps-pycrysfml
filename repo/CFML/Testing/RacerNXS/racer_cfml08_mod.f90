!!----
!!---- Module: Racer_Mod
!!----   Info: Collections of subroutines and global variables used by Racer Program.
!!----
!!---- History
!!----    Update: January - 2010 (Eric Pellegrini)
!!----
!!--..    Encapsulation of the variables related to the configuration in a structure.
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
!!----    Config
!!----    Dx
!!----    Dy
!!----    Expub
!!----    I_Ans
!!----    I_Col
!!----    I_Flg
!!----    I_Fly
!!----    I_Msk
!!----    I_Lib
!!----    I_Raf
!!----    I_Twn
!!----    I_Ubm
!!----    I_Usr
!!----    Icou
!!----    Mask
!!----    Max_Pts
!!----    Racer_CFML_Mod_Error
!!----    Racer_CFML_Mod_Error_Mess
!!----    Stang
!!----    Total
!!----    Vraw
!!----    Vval
!!----    Xmon
!!----    Zgamma
!!----    Zmon
!!----    Zomega
!!----    Ztim
!!----    Xtim
!!----
!!---- Procedures
!!----    Functions:
!!----
!!----    Subroutines:
!!----       Backgr
!!----       Calpos
!!----       Display
!!----       Elplim
!!----       Flag
!!----       Graph
!!----       Libent
!!----       Plan
!!----       Powder
!!----       Refine
!!----       Results
!!----
!!
Module Racer_CFML_Mod

    Use CFML_Globaldeps,     Only : Pi, To_Deg, To_Rad, Dp, err_CFML, clear_error

    Use CFML_Maths,          Only : Inverse_Matrix

    Use CFML_SXTAL_Geom,     Only : cell_fr_UB

    Implicit None

    Private

    Public :: Backgr, Calpos, Default_Configuration, Display, Elplim, Flag, Graph, Libent, &
              Plan, Powder, Read_Detector_Efficiency_Matrix, Refine, Results, Display_IJ

    Integer, Parameter, Public              :: I_Ans=30, I_Usr=15, I_Col=35, I_Raf=36, I_Lib=25, I_Flg=38, I_Ubm=37, &
                                               I_Fly=44, I_Msk=31, I_Twn=39, Max_Pts = 128, I_Ell=32
    Integer, Public                         :: Total
    Integer, Public                         :: n_ver=64,n_hor=64
    Integer, Dimension(:,:), allocatable, Public :: Mask        !(n_ver,n_hor)
    Real, Dimension(:,:,:),  allocatable, Public :: Vraw, Vval  !(n_ver,n_hor,Max_Pts)
    Real, Dimension(:,:),    allocatable, Public :: Alphas
    Real, Dimension(Max_Pts), Public             :: Zmon, Ztim, Zomega, Zgamma
    Real, Public                                 :: Stang, Dx, Dy, Xtim, Xmon
    Real, Dimension(3,3), Public                 :: Expub

    Integer, Dimension(:), Allocatable, Public :: Numors

    Logical, Public                        :: Racer_CFML_Mod_Error = .False.
    Character (Len = 512), Public          :: Racer_CFML_Mod_Error_Mess = ""

    ! Types Declaration.
    ! The Racer configuration type.
    Type, Public :: Racer_Config_Type

        Character (Len=512)                  :: Data_Directory

        Character (Len=512)                  :: newdb_Data_Directory

        Integer                              :: Monitor, Scale, TReg, IAddLib, IRefine, IPrint, ICard, &
                                                Imin, Imax, Jmin, Jmax, Kmin, Kmax, IYearcycle, N_Neigh

        Real                                 :: Cont, DeadTime, ActVol, VolFac, SigBR, Fill, DiffLim, DRad, &
                                                XOffset, YOffset, OmegaOffset, Wavelen
        Real                                 :: Dx, Dy

        Real, Dimension(3,3)                 :: UB, Twin_UB

        Integer                              :: UB_Set, Twin_UB_Set, Wavelen_Set

        Real, Dimension(3,6)                 :: Neigh

        Character (Len=8)                    :: Instrument

        Logical                              :: Racer_nexus !If true, the expected numors are read from NeXus files

        Integer                              :: N_Numors_Intervals,np_hor,np_ver
        Integer, Dimension(:,:), Allocatable :: Numors_Intervals

    End Type Racer_Config_Type
    Type(Racer_Config_Type), Public :: Config

 Contains

    Subroutine Backgr(Minn,Maxx,Map,Bgp,Bgerr,Jflag)

!       ** Returns Background per Detector Element **
!       **  (Constant if Background is Even)       **
        Integer, Dimension(3),     Intent(In)     :: Maxx,Minn
        Integer, Dimension(:,:,:), Intent(In)     :: Map !(n_ver,n_hor,Max_Pts)
        Real,    Dimension(:,:),   Intent(Out)    :: Bgp,Bgerr
        Integer, Dimension(17),    Intent(In Out) :: Jflag
        !
        Integer, Dimension(size(Bgp,1),size(Bgp,2)) :: Npb
        Real                      :: Bgpp, Sqbg, Bgr, Varbg, Qual
        Integer                   :: Nbpp,I,J,K

!         If there are no "Background" points then warn and quit
        Nbpp=0
        Do I=Minn(1),Maxx(1)
            Do J=Minn(2),Maxx(2)
                Do K=Minn(3),Maxx(3)
                    If (Map(I,J,K) == 3) Then
                        Nbpp=Nbpp+1
                    End If
                End Do
            End Do
        End Do

        If (Nbpp == 0) Then
            Jflag(11)=1
            Write(I_Ans,*) ' *** No points in background ellipsoid.',  &
                  ' Approximate background used (from above) ***'
            Return
        End If

        Bgpp=0.0
        Nbpp=0
        Sqbg=0.0

        ! Sum over frames, K-component of Vval
        Do I=Minn(1),Maxx(1)
            Do J=Minn(2),Maxx(2)
                Bgp(I,J)=0.
                Npb(I,J)=0
                Do K=Minn(3),Maxx(3)
                    If (Map(I,J,K) == 3) Then  !Only for background points Map=3
                        Bgpp=Bgpp+Vval(I,J,K)
                        Sqbg=Sqbg+Vval(I,J,K)**2
                        Nbpp=Nbpp+1
                    End If
                End Do
            End Do
        End Do

        Bgr=Sqrt(max(0.0,Bgpp))/Nbpp
        Bgpp=Bgpp/Nbpp
        Sqbg=Sqbg/Nbpp
        Varbg=Sqbg-(Bgpp**2)
        Qual=Varbg/Bgpp

        If (Qual <= 1.5) Then  ! variance(bg)/bg < 1.5 => constant 2D background
            Do I=Minn(1),Maxx(1)
                Do J=Minn(2),Maxx(2)
                    Bgp(I,J)=Bgpp
                    Bgerr(I,J)=Bgr
                End Do
            End Do
            Return                     ! Return with a constant background
        End If

        Jflag(11)=1                    ! Uneven Background
        Write(I_Ans,"(A,F4.1)") ' **Sig(Bg)/Bg=',Qual

        !Calculation of the uneven background by summing Vval over frames
        !for all pixels for which Map is not masked, not in the core and not in the peak
        Do I=Minn(1),Maxx(1)
            Do J=Minn(2),Maxx(2)
                Do K=Minn(3),Maxx(3)
                    If (Map(I,J,K) == 4) Cycle            ! Masked Out.
                    If (Map(I,J,K) == 1) Cycle            ! Core And
                    If (Map(I,J,K) == 2) Cycle            ! Peak
                    Bgp(I,J)=Bgp(I,J)+Vval(I,J,K)
                    Npb(I,J)=Npb(I,J)+1
                End Do
            End Do
        End Do

        Do I=Minn(1),Maxx(1)
            Do J=Minn(2),Maxx(2)

                If (Npb(I,J) < 5) Then
                    Bgp(I,J)=Bgpp
                    Bgerr(I,J)=Bgr
                Else
                    Bgerr(I,J)=Sqrt(max(0.0,Bgp(I,J)))/Npb(I,J)
                    Bgp(I,J)=Bgp(I,J)/Npb(I,J)
                End If

            End Do
        End Do

        Return

    End Subroutine Backgr

    ! Calculates the shift Dp of the peak within the reflection box
    ! corresponding to Dh.
    Subroutine Calpos(Dh,Pmet,Rnu,Dgamdom,Step,Dp0,Dgamma,Domega,Drnu)
	! Arguments
        Real, Dimension(3),   Intent(In)  :: Dh
        Real, Dimension(3,3), Intent(In)  :: Pmet
        Real,                 Intent(In)  :: Rnu,Dgamdom,Step
        Real, Dimension(3),   Intent(Out) :: Dp0
        Real,                 Intent(Out) :: Dgamma,Domega,Drnu

        ! Local Variables
        Real, Dimension(3)                :: Temp

        Temp=Matmul(Pmet,Dh)                ! [Above]*[H]
        Dgamma=Temp(1)
        Domega=Temp(2)
        Drnu=Temp(3)
        Dp0(1)=Drnu*Config%DRad/Dx           ! X <=> Nu <=> Anodes
        Dp0(2)=(Domega*Dgamdom - Dgamma*Cosd(Rnu+Drnu))*Config%DRad/Dy
!                                            ! Y <=> -Gamma <=> Cathodes
        Dp0(3)=(To_Deg*Domega/Step)          ! Z <=> Omega <=> Frames

        Return

    End Subroutine Calpos

    Subroutine Default_Configuration(Cfg)
        !---- Arguments ----!
        Type(Racer_Config_Type), intent(inout) :: Cfg

        Cfg%Cont     = 0.2
        Cfg%Monitor  = 1000
        Cfg%DeadTime = 0.0
        Cfg%Scale    = 0
        Cfg%TReg     = 0

        Cfg%ActVol  = 300.0
        Cfg%VolFac  = 4.0
        Cfg%SigBR   = 1.0
        Cfg%Fill    = 0.8
        Cfg%DiffLim = 0.2

        Cfg%IAddLib = 0
        Cfg%IRefine = 100
        Cfg%IPrint  = 3
        Cfg%ICard   = 0

        Cfg%IMin =  2
        Cfg%IMax = 30
        Cfg%JMin =  2
        Cfg%JMax = 30
        Cfg%KMin =  1
        Cfg%KMax = 30

        Cfg%UB     = 0.0
        Cfg%UB_Set = 0

        Cfg%Twin_UB     = 0.0
        Cfg%Twin_UB_Set = 0

        Cfg%Neigh   = 0.0
        Cfg%N_Neigh =  0

        Cfg%DRad         = 488.0
        Cfg%XOffset      =   0.0
        Cfg%YOffset      =   0.0
        Cfg%OmegaOffset  =   0.0
        Cfg%Wavelen      =  -1.00
        Cfg%Wavelen_Set  =   0

        Cfg%Data_Directory = " "
        Cfg%newdb_Data_Directory = " "
        Cfg%Instrument        = "d9"
        Cfg%np_hor            = 32
        Cfg%np_ver            = 32
        Cfg%Dx                = 2.0
        Cfg%Dy                = 2.0

        Cfg%IYearcycle = -1

        Cfg%N_NUMORS_INTERVALS = 0

        If (Allocated(Cfg%Numors_Intervals)) Deallocate(Cfg%Numors_Intervals)

    End Subroutine Default_Configuration

    Subroutine Display(Minn,Maxx,Map)

!       * Writes contents of map and observed counts for elements *
        Integer, Dimension(3),     Intent (In) :: Minn,Maxx
        Integer, Dimension(:,:,:), Intent (In) :: Map ! max(64,64,Max_Pts)
        Character(Len=3*n_hor+3) :: Ibuf1
        Character(Len=n_hor+3) :: Ibuf2
        Integer :: Nj,Nj1,Nj2,K,I,J
        Character(len=9) :: forma1,forma2
        forma1="(a,   I1)"
        forma2="(a,   I3)"
        write(forma1(4:6),"(i3)") n_hor
        write(forma2(4:6),"(i3)") n_hor

        Write(I_Ans,"(/)")

        Nj=Maxx(2)-Minn(2)+1
        Nj1=3*Nj+1
        Nj2=Nj+1

        Do K=Minn(3),Maxx(3)
            Write(I_Ans,"(/,5(A,I4),A)") '  Frame  ',K,'  rows run down page from',7+Maxx(1),'  to',7+Minn(1), &
                                   '   columns from ',7+Minn(2),' To ',7+Maxx(2),' across page.'

            Do I=Maxx(1),Minn(1),-1
                Write(Ibuf1,forma2) '   ',(Nint(Vval(I,J,K)),J=Minn(2),Maxx(2))
                Write(Ibuf2,forma1) ' ',(Map(I,J,K),J=Minn(2),Maxx(2))
                Write(I_Ans,"(2A)") Ibuf1(1:Nj1),Ibuf2(1:Nj2)
            End Do

        End Do

        Return

    End Subroutine Display

    Subroutine Display_IJ(i_lun)
        Integer,                   Intent (In) :: i_lun
        Character(Len=128) :: Ibuf
        Integer :: K,I,J
        real :: maxint
        Character(len=9) :: forma
        real, dimension(n_ver,n_hor) :: norm_int
        forma="(a,   I1)"
        write(forma(4:6),"(i3)") n_hor
        !write(*,*) " n_ver =",n_ver," n_hor =",n_ver
        !write(*,*) "  => "//trim(forma)
        !write(*,*) "  => Size i,j,k ",size(Vval,1),size(Vval,2),size(Vval,3)
        !Write(i_lun,"(/)")
        norm_int=0
        do k=Config%kmin,Config%kmax
          norm_int=norm_int+abs(Vval(:,:,k))
        end do
        maxint =maxval(norm_int)
        norm_int=9.0*norm_int/maxint
        Do i=1,n_ver
            write(Ibuf,forma) "  ",nint(norm_int(i,1:n_hor))
            Write(i_lun,"(A)") trim(Ibuf)
        End Do
        write(i_lun,*) "  "
        Do j=1,n_hor
            write(Ibuf,forma) "  ",nint(norm_int(1:n_ver,j))
            Write(i_lun,"(A)") trim(Ibuf)
        End Do

        Return
    End Subroutine Display_IJ

    Subroutine Elplim(Elps,Ihi,Xcen,Xvol,Maxx,Minn,Jflag)
!    *****************************************************
!    *  Delivers in max,min the upper and lower anode,   *
!    *  cathode,frame limits of volume Xvol*Elps about   *
!    *  xcen and flags attempted excursions beyond data. *
!    *****************************************************
        Real,    Dimension(3,3), Intent (In)     :: Elps
        Integer, Dimension(3),   Intent (In)     :: Ihi
        Integer, Dimension(3),   Intent (Out)    :: Maxx,Minn
        Real,    Dimension(3),   Intent (In)     :: Xcen
        Integer, Dimension(17),  Intent (In Out) :: Jflag

        Real, Dimension(3,3)   :: Sple
        Real, Dimension(3  )   :: Ext
        Logical                :: Singular
        Real                   :: Xvol,X
        Integer                :: I

        Jflag(1:6) = 0
        X = Xvol**(1.0/3.0)

        !Call Invert_Matrix(Elps,Sple,Singular)
        singular=.false.
        Sple=Inverse_Matrix(Elps)
        if(err_CFML%Ierr /= 0) then
            singular=.true.
            write(*,"(a)") " => "//trim(err_CFML%Msg)
            call Clear_Error()
        end if
        Do I=1,3
            Ext(I)=X*Sqrt(Sple(I,I))
        End Do

        Do I=1,3
            Minn(I)=Int(Xcen(I)-Ext(I)+1)
            Maxx(I)=Int(Xcen(I)+Ext(I))

            If (Minn(I) < 1) Then           ! Check Inside Data Limit,
                Minn(I)=1
                Jflag(I)=1                  ! if not, set Jflag
            End If

            If (Maxx(I) > Ihi(I)) Then
                Maxx(I)=Ihi(I)              ! and set at limit.
                Jflag(I+3)=1
            End If

        End Do

        Return

    End Subroutine Elplim

    Subroutine Flag(Jflag,Numor)

        Integer, Dimension(17),Intent(In) :: Jflag
        Integer,               Intent(In) :: Numor

        Character(Len=4),   Dimension(17) :: Lin
        Integer :: J
        integer, save :: i_cou=0

        i_cou=i_cou+1
        Do J=1,17
            Lin(J)='   .'
        End Do

        Do J=1,17
            If (Jflag(J) == 1) Lin(J)='   *'
        End Do

        Lin(7)='   W'

        If (Jflag(7) == 1) Lin(7)='   S'

        Write(I_Flg,"(1X,I6,17A4,tr4,i6)") Numor,(Lin(J),J=7,14),(Lin(J),J=1,6), Lin(15),Lin(16),Lin(17),i_cou
        Write(I_Ans,"(A,17I4,tr4,i6)") '  Numor  F1',(J,J=2,17),i_cou
        Write(I_Ans,"(1X,I6,17A4,tr4,i6)") Numor,(Lin(J),J=7,14),(Lin(J),J=1,6), Lin(15),Lin(16),Lin(17),i_cou
        Write(*,"(1X,I6,17A4,tr4,i6)") Numor,(Lin(J),J=7,14),(Lin(J),J=1,6), Lin(15),Lin(16),Lin(17),i_cou

        Return

    End Subroutine Flag

    Subroutine Graph(Cnt,Cnterr,Frame,Maxx,Minn)

        !       ** Line printer graph of peaks **
        Real,    Dimension(Max_Pts),Intent(In) :: Cnt,Cnterr,Frame
        Integer, Dimension(3),      Intent(In) :: Maxx,Minn
        Character(Len=1), Dimension(55) :: Lin

        Real    :: Cmin, Cmax,Rangec, Scal
        Integer :: K,Nzero,Npoint,Nframe,Nerr,N

        Write(I_Ans,"(/,A,/,A)") ' Omega scan of frames in integration . = Background Box, * = Peak Ellipsoid', &
                            ' Step  Omega'

        Cmin=0.0
        Cmax=0.0

        Do K = Minn(3),Maxx(3)
            If (Cnt(K) < Cmin) Cmin=Cnt(K)          ! Find Min,Max
            If (Cnt(K) > Cmax) Cmax=Cnt(K)
            If (Frame(K) < Cmin) Cmin=Frame(K)      ! Of All Points
            If (Frame(K) > Cmax) Cmax=Frame(K)
        End Do

        Rangec=Cmax-Cmin
        Scal=50.0/Rangec                          ! Scale To 50 Columns
        Nzero=Nint(-Cmin*Scal)+1                  ! Set Zero Level

        If (Cmin > 0) Nzero=1

        Do K=Minn(3),Maxx(3)
            Lin(1:55)=' '                           ! Clear Columns
            Lin(Nzero)='!'                          ! Draw Axis
            Npoint=Int(Scal*Cnt(K))+Nzero
            Nframe=Int(Scal*Frame(K))+Nzero
            Nerr=Nint(Scal*Cnterr(K))
            Lin(Nframe)='.'                         ! Draw Box Contents

            Do N=1,Nerr
                If ((Npoint+N) <= 55) Lin(Npoint+N)='-'  ! Draw Error Bars
                If ((Npoint-N) < 1) Cycle
                Lin(Npoint-N)='-'
            End Do

            If ((Npoint+Nerr) <= 55) Lin(Npoint+Nerr)='I'        !
            If ((Npoint-Nerr) >= 1)  Lin(Npoint-Nerr)='I'
            Lin(Npoint)='*'                         ! Plot Point
            Write(I_Ans,"(1X,I3,F8.2,1X,I10,1X,55A1)") K+Config%Kmin-1,Zomega(K+Config%Kmin-1),Nint(Cnt(K)),Lin
        End Do

        Return

    End Subroutine Graph

    Subroutine Libent(Pklib,Pk,N)
!     * Make Peak Library Entry At Position N In List *
        Real, Dimension(1000,13), Intent(In Out) :: Pklib
        Real, Dimension(13),      Intent(In)     :: Pk
        Integer,                  Intent(In)     :: N
        Integer     :: L

        Do L=1,13
            Pklib(N,L)=Pk(L)
        End Do

        Return

    End Subroutine Libent

    Subroutine Plan(Elps,Xcen,Vol,Map,Maxx,Minn,Jmask,Jflag)
!    ********************************************************
!    *  Returns (in map) a plan of the data divided into 5  *
!    *  Volumes(1=Core,2=Peak,3=Background)                 *
!    *  (0= Outside Background, 4= Masked out by Imask)     *
!    ********************************************************
        Real,    Dimension(3,3),           Intent (In)     :: Elps
        Real,    Dimension(3),             Intent (In)     :: Xcen,Vol
        Integer, Dimension(:,:,:), Intent (In Out) :: Map !max(64,64,Max_Pts)
        Integer, Dimension(3),             Intent (In)     :: Maxx,Minn
        Integer, Dimension(:,:),           Intent (In)     :: Jmask
        Integer, Dimension(17),            Intent (In Out) :: Jflag
        !--- Local Variables
        Real,    Dimension(3)     :: Vec,X,C
        Integer                   :: I,J,K,M,N
        Real                      :: Fel

        X(1)=(Vol(1))**(2.0/3.0)          ! Core      }
        X(2)=(Vol(2))**(2.0/3.0)          ! Peak      } Volume
        X(3)=(Vol(3))**(2.0/3.0)          ! Background} Multipliers

        Do J=Minn(2),Maxx(2)
            Do I=Minn(1),Maxx(1)
                Do K=Minn(3),Maxx(3)

                    Vec(1) = Real(I)-Xcen(1)
                    Vec(2) = Real(J)-Xcen(2)                  ! Vector From
                    Vec(3) = Real(K)-Xcen(3)                  ! Ellipsoid Centre

                    Do M=1,3
                        C(M)=0.0

                        Do N=1,3
                            C(M)=C(M)+Elps(M,N)*Vec(N)       ! Calculates Right
                        End Do                               ! Hand Side Of

                    End Do                                   ! Hand Side Of

                    Fel=C(1)*Vec(1)+C(2)*Vec(2)+C(3)*Vec(3) ! Ellipsoid Equation

                    Do N=3,1,-1

                        If (Fel < X(N)) Then                  ! Vector Inside Vol(N)

                            If (Map(I,J,K) /= 4) Then             ! Only Label If Not Masked
                                Map(I,J,K)=N                      ! As A Neighbouring Refl.
                            Else If (N == 1 .Or. N == 2) Then
                                Jflag(15)=1
                            End If

                        End If

                    End Do

                End Do

            End Do

        End Do

!         Mask Out Elements
        Do K=1,Max_Pts
            Do J=1,n_hor
                Do I=1,n_ver

                    If (Jmask(I,J) == 0) Then
                        If (Map(I,J,K) == 1) Jflag(15)=1
                        If (Map(I,J,K) == 2) Jflag(15)=1
                        Map(I,J,K)=4
                    End If

                End Do
            End Do
        End Do

        Return

    End Subroutine Plan

    Subroutine Powder(Map,Gamma,Rnu,Xcalc,Ycalc,Zcalc,Dgamdom, Step)

!       ** Averages the counts bounding the reflection box in 2Theta **
!       ** to eliminate powder lines, and corrects all counts        **
!       ** accordingly.                                              **
        Integer, Dimension(:,:,:), Intent(In) :: Map !(64,64,Max_Pts)
        Real,                              Intent(In) :: Gamma,Rnu
        Real,                              Intent(In) :: Xcalc,Ycalc,Zcalc
        Real,                              Intent(In) :: Dgamdom, Step

        ! Parameters
        Integer, Parameter :: Max_Nbg = 400

        ! Local Variables
        Real,    Dimension(Max_Nbg) :: Bg2Th
        Integer, Dimension(Max_Nbg) :: Nbg2Th
        Integer                     :: I,J,K,Nave,Ii,Jj,Kk,Nbg,Nth,N,N1,N2
        Real                        :: Tthscn,Dthdga,Dthdnu,Dxdrd,Dydrd,Tthmin, &
                                       Tthmax,Tthstp,Tth, X,Y,Z,Avebg

        ! Zero the 2theta background arrays
        Bg2Th=0.0; Nbg2Th = 0

        ! Calculate 2Theta, some derivatives, and some constants
        Tthscn = Acosd(Cosd(Gamma)*Cosd(Rnu))
        Dthdga = Sind(Gamma)*Cosd(Rnu)/Sind(Tthscn)
        Dthdnu = Cosd(Gamma)*Sind(Rnu)/Sind(Tthscn)
        Dxdrd = 180.0*Dx/(Config%DRad*Pi)
        Dydrd = 180.0*Dy/(Config%DRad*Pi)

        ! Find minimum and maximum 2Theta of reflection box, ignoring
        ! the possibility that some corners may be masked off.
        ! tthscn is the 2Theta value at the centre of the scan box.
        ! Remember that in racer X is along Nu, Y is along gamma and
        ! Z is along omega.

        Tthmin = 180.0
        Tthmax = 0.0
        Tthstp = 0.1

        Do I = 1, 2
            X = -(I-2)*Config%Imin + (I-1)*Config%Imax
            Do J = 1,2
                Y = -(J-2)*Config%Jmin + (J-1)*Config%Jmax

                Do K = 1,2
                    Z = -(K-2)*Config%Kmin + (K-1)*Config%Kmax
                    Tth = Tthscn + (X-Xcalc)*Dxdrd*Dthdnu  &
                          - ((Y-Ycalc)*Dydrd - Dgamdom*(Z-Zcalc)*Step)*Dthdga
                    Tthmin = Amin1(Tthmin,Tth)
                    Tthmax = Amax1(Tthmax,Tth)
                End Do

            End Do
        End Do

        Nbg = (Tthmax - Tthmin)/Tthstp + 1

        If (Nbg > Max_Nbg) Return

!         Step through all points outside the expected peak volume
!         and add to the 2Theta background profile.  Assume that the
!         peak occupies the middle half of the box in each direction.
!         Remember that Vval contains just the counts in the reflection
!         box.  Therefore we must subtract Imin,Jmin,Kmin.

        Do I = Config%Imin,Config%Imax
            Ii = I-Config%Imin+1
            Do J = Config%Jmin,Config%Jmax
                Jj = J-Config%Jmin+1
                Do K = Config%Kmin,Config%Kmax
                    Kk = K-Config%Kmin+1

                    If ((I < (3*Config%Imin+Config%Imax)/4 .Or. I > (Config%Imin+3*Config%Imax)/4 .Or.   &
                         J < (3*Config%Jmin+Config%Jmax)/4 .Or. J > (Config%Jmin+3*Config%Jmax)/4 .Or.   &
                         K < (3*Config%Kmin+Config%Kmax)/4 .Or. K > (Config%Kmin+3*Config%Kmax)/4) .And. &
                         Map(I,J,K) /= 4) Then

                        Tth = Tthscn + (I-Xcalc)*Dxdrd*Dthdnu  &
                              - ((J-Ycalc)*Dydrd - Dgamdom*(K-Zcalc)*Step)*Dthdga

                        Nth = (Tth - Tthmin)/Tthstp + 1

                        If (Nth > 0 .And. Nth < Max_Nbg+1) Then
                            Bg2Th(Nth) = Bg2Th(Nth) + Vval(Ii,Jj,Kk)
                            Nbg2Th(Nth) = Nbg2Th(Nth) + 1
                        End If

                    End If

                End Do

            End Do

        End Do

        ! Normalise the 2Theta background profile

        Avebg=0.0
        Nave=0

        Do I = 1,Nbg
            If (Nbg2Th(I) /= 0) Then
                Bg2Th(I) = Bg2Th(I)/Nbg2Th(I)
                Avebg = Avebg +Bg2Th(I)
                Nave = Nave + 1
            End If
        End Do

        If (Nave > 0) Avebg = Avebg/Nave

        ! Extrapolate and interpolate to fill in the gaps in the 2Theta
        ! profile.

        ! Find the first occupied element and 'Extrapolate' this value back
        ! to the start.

        N = 1
        Do While (Nbg2Th(N) == 0 .And. N <= Size(Nbg2Th))
            N = N+1
        End Do

        N1 = N

        If (N1 > 1) Then
            Do I = 1, N1 - 1
                Bg2Th(I) = Bg2Th(N1)
            End Do
        End If

!         Similarly for the last elements of the 2Theta profile

        N = -1
        N = Nbg
        Do While (Nbg2Th(N) == 0 .And. N >= 0)
            N = N - 1
        End Do

        N2 = N

        If (N2 < Nbg) Then
            Do I = N2 + 1, Nbg
                Bg2Th(I) = Bg2Th(N2)
            End Do
        End If

        !  Now interpolate the points between N1 and N2

        Do While (N1 < N2)
            N = N1+1

            Do While (Nbg2Th(N) == 0 .And. N < N2+1)
                N = N+1
            End Do

            If (N > N1+1) Then
                Do I = N1+1,N-1
                    Bg2Th(I)=Bg2Th(N1)+(Bg2Th(N)-Bg2Th(N1))*(I-N1)/(N-N1)
                End Do
            End If

            N1 = N

        End Do

        ! Now correct all points in the Reflection Box
        Do I = Config%Imin,Config%Imax

            Ii = I-Config%Imin+1

            Do J = Config%Jmin,Config%Jmax

                Jj = J-Config%Jmin+1

                Do K = Config%Kmin,Config%Kmax

                    Kk = K-Config%Kmin+1

                    If (Map(I,J,K) /= 4) Then

                        Tth = Tthscn + (I-Xcalc)*Dxdrd*Dthdnu  &
                              - ((J-Ycalc)*Dydrd - Dgamdom*(K-Zcalc)*Step)*Dthdga

                        Nth = Nint((Tth - Tthmin)/Tthstp + 1.)

                        If (Nth > 0 .And. Nth < Nbg+1) Then
                            Vval(Ii,Jj,Kk) = Vval(Ii,Jj,Kk) - Bg2Th(Nth) + Avebg
                        End If

                    End If

                End Do

            End Do

        End Do

        Return

    End Subroutine Powder

    Subroutine Read_Detector_Efficiency_Matrix(Alphas_Filename)
        !---- Arguments ----!
        Character (Len=*), Intent(In) :: Alphas_Filename

        !---- Local Variables ----!
        Integer :: Ier

        Racer_CFML_Mod_Error = .False.
        Racer_CFML_Mod_Error_Mess = ""

        Open(Unit = 16, File = Alphas_Filename, Status = 'Old', Action = 'Read', Iostat = Ier)
        If (Ier /= 0) Then

            Alphas = 1.0

            Racer_CFML_Mod_Error = .True.
            Racer_CFML_Mod_Error_Mess = 'The detector efficiency matrix file could not be opened.&
            & Racer will consider an uniform efficiency of 1.'

            Return

        End If

        Read(Unit = 16, Fmt = *, Iostat = Ier) Alphas

        If (Ier /= 0) Then
            Alphas = 1.0
            Racer_CFML_Mod_Error = .True.
            Racer_CFML_Mod_Error_Mess = 'The detector efficiency matrix file could not be read properly.&
            & Racer will consider an uniform efficiency of 1.'

        End If

        Close(Unit = 16)

        Return

    End Subroutine Read_Detector_Efficiency_Matrix

    Subroutine Refine(Cne,Dne,Dpar,Machine)
!       * Predict changes in UB, offsets and write ubfrom.dat *

        Real, Dimension(12,12), Intent (In Out) :: Cne
        Real, Dimension(12),    Intent (In Out) :: Dne
        Real, Dimension(12),    Intent (   Out) :: Dpar
        Integer,                Intent (In)     :: Machine

        Integer                :: Mm,I,J,M,N
        Real                   :: Yyoffset,Xxoffset,Zzoffset
        Real, Dimension(12,12) :: Enc
        Real, Dimension(3,3)   :: ub
        Logical                :: Singular
        real, Dimension(6)     :: dcel,rcel
        character(len=*),dimension(3),parameter :: UB_Txt= (/"BL_UB(1,1),BL_UB(1,2),BL_UB(1,3)", &
                                                    "BL_UB(2,1),BL_UB(2,2),BL_UB(2,3)", &
                                                    "BL_UB(3,1),BL_UB(3,2),BL_UB(3,3)"/)

        Mm=12
        If (Machine == 1) Then
            Dne(11)=Dne(12)

            Do I=1,9
                Cne(I,11)=Cne(I,12)                    ! Normal Beam
                Cne(I,12)=0.0                          ! No Domega
            End Do

        End If

        Do I=1,12
            Do J=1,I
                Cne(I,J)=Cne(J,I)
            End Do
        End Do

        !Call Invert_Matrix(Cne,Enc,Singular)                 ! Invert Normal Eqs.
        singular=.false.
        Enc=Inverse_Matrix(Cne)
        if(err_CFML%Ierr /= 0) then
            singular=.true.
            write(*,"(a)") " => "//trim(err_CFML%Msg)
            call Clear_Error()
        end if

        If (Machine == 1) Mm=11

        Do M=1,Mm
            Dpar(M)=0.0
            Do N=1,Mm
                Dpar(M)=Dpar(M)+Enc(M,N)*Dne(N)        ! Multiply Diffs.
            End Do

        End Do

        Yyoffset=Config%YOffset-Dpar(10)*Config%DRad
        Xxoffset=Config%XOffset+Dpar(Mm)*Config%DRad

        If (Machine == 0) Zzoffset=Config%OmegaOffset+To_Deg*Dpar(11)

        Write(I_Ubm,"(4F10.3,t65,a)") Config%DRad,Yyoffset,-Xxoffset,Zzoffset, "! Dist. sample-detector, Y-off, X-off, Z-off"

        Do N=1,3
            do m=1,3
              ub(N,M)=Config%UB(N,M)+Dpar(3*(N-1)+M)
            end do
            !Write(I_Ubm,"(3F15.7,t50,a)") (Config%UB(N,M)+Dpar(3*(N-1)+M),M=1,3),"!  "//UB_Txt(N)
            Write(I_Ubm,"(3F15.7,t65,a)") ub(N,:),"!  "//UB_Txt(N)
        End Do
        Call cell_fr_UB(ub,dcel=dcel,rcel=rcel)
        write(I_Ubm,"(3f10.5,3f10.4,t65,a)") dcel, "! Direct     cell parameters"
        write(I_Ubm,"(3f10.6,3f10.4,t65,a)") rcel, "! Reciprocal cell parameters"
        !Write(I_Ubm,"(/,/)")
        Dne(:)=0.0             ! Flush out
        Cne(:,:)=0.0           ! matrices

    End Subroutine Refine

    Subroutine Results(Numor,Hproc,Peak,Perr,Rnu,Gamma,Temper,  &
        Ncol,Icard,Omega,Chi,Phi,Step,Stang,Smon,Jflag,Machine)

        Integer,                 Intent (In) :: Numor,Machine,Icard,Ncol
        Real,    Dimension(3),   Intent (In) :: Hproc
        Integer, Dimension(17),  Intent (In) :: Jflag
        Real,                    Intent (In) :: Peak,Perr,Rnu,Gamma,Temper
        Real,                    Intent (In) :: Omega,Chi,Phi,Step,Stang,Smon
        !     * Output results in Coll5N format  *
        Character(Len=1) :: Cardop
        Real             :: Corr,Fsq,Dfsq,Anglev      ! J.Schefer   Mai 93
        Integer          :: I

        Cardop='A'

        If (Icard >= 1) Cardop='R'

        Corr=Sind(Gamma)*Cosd(Rnu)
        Corr=Corr*Step/Stang    ! Lorentz and step normalisation
        Corr=Corr*Smon          ! Monitor or time scaling
        ! Write(I_Ans,"(' In Results.',7F10.5)") Peak, Corr, Gamma, Rnu, Step, Stang, Smon
        Fsq=Peak*Corr
        Dfsq=Perr*Corr

        If (Machine == 0) Then
            Anglev=Chi
        Else
            Anglev=Rnu
        End If

        If (Icard == 1) Then
            Write(Ncol,"(I6,3F6.2,I8,I4,4F8.2,F6.1,5X,A1)")                &
                        Numor,(Hproc(I),I=1,3),Nint(Fsq),Nint(Dfsq),  &
                        0.5*Gamma,Omega,Anglev,Phi,Temper,Cardop
        Else If (Icard == 2) Then
            Write(Ncol,"(I6,3F9.4,I8,I4,4F8.2,F6.1,5X,A1)")                &
                        Numor,(Hproc(I),I=1,3),Nint(Fsq),Nint(Dfsq),  &
                        0.5*Gamma,Omega,Anglev,Phi,Temper,Cardop
        Else
            Write(Ncol,"(I6,3I4,2F10.2,4F8.2,F9.2,A1)")              &
                         Numor,(Int(Hproc(I)),I=1,3),Fsq,Dfsq,  &
                         0.5*Gamma,Omega,Anglev,Phi,Temper,Cardop
        End If

        Write(I_Ans,"(A,I7,A,I4,A)")' Lorentz Corrected Intensity is',Nint(Fsq),' (',Nint(Dfsq),')'

        Call Flag(Jflag,Numor)

        Return

    End Subroutine Results

End Module Racer_CFML_Mod
