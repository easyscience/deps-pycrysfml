Module nFP_globals

   !---- Use Modules ----!
   use CFML_GlobalDeps,  only: cp
   use CFML_gSpaceGroups,only: rot_mat_type
   use CFML_IOForm,      only: Phase_Type
   use CFML_DiffPatt,    only: Pattern_Type, Interval_Type, Bck_Type, Excl_reg_type
   use CFML_Reflections, only: RefList_Type, refl_type, RefP_type
   use CFML_KeyCodes,    only: GenParList_Type
   use CFML_Powder

   !---- Variables ----!
   implicit none
   public


   type(GenParList_Type)                         :: VGen      !> General vector containing refinement codes
   Type(rot_mat_type), dimension(:), allocatable :: rot_mats

   Type, public :: profile_contrib
     integer                                :: n_pts
     Real(kind=cp),dimension(:),allocatable :: yp  !(npts)
   End Type profile_contrib

   Type, public :: profile_contrib_phase
     integer                                        :: n_patts
     Type(profile_contrib),dimension(:),allocatable :: prc  !(n_patts)
   End Type profile_contrib_phase

   Type(profile_contrib_phase), dimension(:),allocatable  :: prph
   !
   !----
   !!---- TYPE :: SREFL_TYPE
   !!--..
   !!
   !Type, public, extends(Refl_Type) :: SRefl_Type -> MRefl_Type
   ! Basic components of Refl_Type
   !   integer,dimension(:), allocatable :: H             ! H
   !   integer                           :: Mult  = 0     ! Mutiplicity
   !   real(kind=cp)                     :: S     = 0.0   ! Sin(Theta)/lambda=1/2d
   !   integer                           :: Imag  = 0     ! 0: nuclear reflection, 1:magnetic, 2=both
   !   integer                           :: Pcoeff= 0     ! Pointer to the harmonic q_coeff
   ! Extra components for SRefl_Type
   !   real(kind=cp)              :: Fo    = 0.0_cp  !> Observed Structure Factor
   !   real(kind=cp)              :: Fc    = 0.0_cp  !> Calculated Structure Factor
   !   real(kind=cp)              :: SFo   = 0.0_cp  !> Sigma of  Fo
   !   real(kind=cp)              :: Phase = 0.0_cp  !> Phase in degrees
   !   real(kind=cp)              :: A     = 0.0_cp  !> real part of the Structure Factor
   !   real(kind=cp)              :: B     = 0.0_cp  !> Imaginary part of the Structure Factor
   !   real(kind=cp)              :: W     = 1.0_cp  !> Weight factor
   !   real(kind=cp),dimension(3) :: Hr              !> Real 3D indices of the reflection (Non-integer for incommensurate structures).
   ! Extra components for MRefl_Type
   !   real(kind=cp)                  :: mIvo  =0.0_cp               ! Observed modulus of the Magnetic Interaction vector
   !   real(kind=cp)                  :: smIvo =0.0_cp               ! Sigma of observed modulus of the Magnetic Interaction vector
   !   complex(kind=cp), dimension(3) :: msF   =cmplx(0.0_cp,0.0_cp) ! Magnetic structure factor w.r.t. unitary Crystal Frame
   !   complex(kind=cp), dimension(3) :: mIv   =cmplx(0.0_cp,0.0_cp) ! Magnetic interaction vector w.r.t. unitary Crystal Frame
   !   complex(kind=cp), dimension(3) :: MiVC  =0.0                  ! Magnetic interaction vector in Cartesian components w.r.t. Crystal Frame
   !   real(kind=cp)                  :: sqMiV =0.0                  ! Square of the Magnetic Interaction vector
   !End Type SRefl_Type

   !Type, public :: RefList_Type
   !   integer                                     :: NRef=0 ! Number of Reflections
   !   class(refl_type), dimension(:), allocatable :: Ref    ! Reflection List
   !End Type RefList_Type

   Logical :: Simulation           !> If .true. no need of experimental data, just calculation
   Logical :: Global_Optimization  !> One of the global optimization algorithms is applied
   Integer :: Opt_algor            !> Number of the algorithm for optimization (component of Algor)
   Character(len=*), dimension(12), parameter :: Algor=["gauss_newton         ", & !GANEW
                                                        "levenberg_marquardt  ", & !LEVMAR
                                                        "marquardt_fit        ", & !MARFIT
                                                        "orthog-dist-regresion", & !ODR
                                                        "conjugate_gradient   ", & !CONGRD
                                                        "bfgs_quasi_newton    ", & !BFGS
                                                        "simplex              ", & !SIMP
                                                        "dfp_no-derivatives   ", & !DFP
                                                        "global_csendes       ", & !GCS
                                                        "local_random         ", & !LRAMD
                                                        "simulated_annealing  ", & !SANN
                                                        "unirandi             "]   !UNIRANDI


End Module nFP_globals
