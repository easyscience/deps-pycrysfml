!!-------------------------------------------------------
!!---- Crystallographic Fortran Modules Library (CrysFML)
!!-------------------------------------------------------
!!---- The CrysFML project is distributed under LGPL. In agreement with the
!!---- Intergovernmental Convention of the ILL, this software cannot be used
!!---- in military applications.
!!----
!!---- Copyright (C) 1999-2022  Institut Laue-Langevin (ILL), Grenoble, FRANCE
!!----                          Universidad de La Laguna (ULL), Tenerife, SPAIN
!!----                          Laboratoire Leon Brillouin(LLB), Saclay, FRANCE
!!----
!!---- Authors: Juan Rodriguez-Carvajal (ILL)
!!----          Javier Gonzalez-Platas  (ULL)
!!----          Nebil Ayape Katcho      (ILL)
!!----
!!---- Contributors: Laurent Chapon     (ILL)
!!----               Marc Janoschek     (Los Alamos National Laboratory, USA)
!!----               Oksana Zaharko     (Paul Scherrer Institute, Switzerland)
!!----               Tierry Roisnel     (CDIFX,Rennes France)
!!----               Eric Pellegrini    (ILL)
!!----               Ross Angel         (University of Pavia)
!!----
!!---- This library is free software; you can redistribute it and/or
!!---- modify it under the terms of the GNU Lesser General Public
!!---- License as published by the Free Software Foundation; either
!!---- version 3.0 of the License, or (at your option) any later version.
!!----
!!---- This library is distributed in the hope that it will be useful,
!!---- but WITHOUT ANY WARRANTY; without even the implied warranty of
!!---- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!!---- Lesser General Public License for more details.
!!----
!!---- You should have received a copy of the GNU Lesser General Public
!!---- License along with this library; if not, see <http://www.gnu.org/licenses/>.
!!----
!!----
!!---- MODULE: CFML_Profiles
!!
Module CFML_Profiles
    !---- Use Modules ----!
    Use CFML_GlobalDeps, only: CP, DP, PI, to_rad, to_deg
    Use CFML_Maths, only: erfc_deriv

    !---- Variables ----!
    implicit none

    private

    !---- List of public Subroutines ----!
    public :: init_prof_val, prof_val, Calc_Pseudo_Voigt, Prof_Lorentzian, Prof_Gaussian, &
              psvoigtian, Get_HG_HL, Get_FWHM_Eta, Keijser

    !---- List of public functions ----!
    public :: Pseudovoigt,Lorentzian,Gaussian,Back_To_Back_Exp, Ikeda_Carpenter, Exponential, &
              Hat, Split_Pseudovoigt, TCH_pVoigt

    public :: Pseudovoigt_Der,Lorentzian_Der,Gaussian_Der,Back_To_Back_Exp_Der, Ikeda_Carpenter_Der, &
              Exponential_Der, Hat_Der, Split_Pseudovoigt_Der, TCH_pVoigt_Der

    public :: Tof_Jorgensen, Tof_Jorgensen_Vondreele, Tof_Carpenter

    public :: Expi_E1


    !---- Definitions ----!
    logical,         public  :: init_profval=.false.
    logical,         public  :: Lorcomp=.false.    ! .true. if there are Lorentzian components

    real(kind=dp), parameter :: EXP_M=0.003_dp
    real(kind=dp), parameter :: INV_8LN2=0.18033688011112042591999058512524_dp ! use for scaling the divisions
    real(kind=dp), parameter :: TWO_OVER_PI=0.63661977236758134307553505349006_dp
    real(kind=cp), parameter :: PI_OVER_TWO=0.5_cp*pi
    real(kind=cp), parameter :: eps=1.0e-6_cp
    integer,       parameter :: CTRL_NSTEPS= 1000

    integer, dimension(14)   :: nterms =[6,10,20,40,60,80,100,150,200,300,400,600,800,1000]
    integer, dimension(14)   :: fstterm=[0, 3, 8,18,38,68,108,158,233,333,483,683,983,1383]

    real(kind=cp),dimension(0:1883) :: wp=0.0_cp ! Storage for Gauss-Legendre weights
    real(kind=cp),dimension(0:1883) :: xp=0.0_cp ! Storage for Gauss-Legendre intervals

    real(kind=cp), save      :: twoth0_prev = 0.0_cp  ! Variables to switch to new calculations of some variables
    real(kind=cp), save      ::  asym1_prev = 0.0_cp
    real(kind=cp), save      ::  asym2_prev = 0.0_cp

    !!----
    !!---- TYPE :: DERIV_TOF_TYPE
    !!--..
    !!
    Type, Public :: Deriv_TOF_Type
       real(kind=cp) :: alfa  =0.0_cp   ! omega_a  DOmega/Dalpha
       real(kind=cp) :: beta  =0.0_cp   ! omega_b  DOmega/Dbeta
       real(kind=cp) :: dt    =0.0_cp   ! omega_t  DOmega/Ddt      (dt=TOFi-TOF(Bragg))
       real(kind=cp) :: sigma =0.0_cp   ! omega_s  DOmega/Dsigma   (for tof_Jorgensen function)
       real(kind=cp) :: gamm =0.0_cp   ! omega_g  DOmega/Dgamm   (for tof_Jorgensen_VonDreele function)
       real(kind=cp) :: eta   =0.0_cp   ! omega_e  DOmega/Deta                     "
       real(kind=cp) :: kappa =0.0_cp   ! omega_e  DOmega/kappa    (for tof_Carpenter function)
    End Type Deriv_TOF_Type

    Interface
       Pure Module Function Back_To_Back_Exp(X,Par) Result (Bb_Val)
          !---- Arguments ----!
          real(kind=cp),              intent(in) :: x
          real(kind=cp), dimension(:),intent(in) :: par
          real(kind=cp)                          :: bb_val
       End Function Back_To_Back_Exp

       Pure Module Subroutine Back_To_Back_Exp_Der(X,Par,Bb_Val,Dpar)
          !---- Arguments ----!
          real(kind=cp),                        intent(in)  :: x
          real(kind=cp),           dimension(:),intent(in)  :: par
          real(kind=cp),                        intent(out) :: bb_val
          real(kind=cp), optional, dimension(:),intent(out) :: dpar
       End Subroutine Back_To_Back_Exp_Der

       Module Subroutine Calc_Pseudo_Voigt(x,y,Twoth0,Eta,Fwhm,asym1,asym2)
          !---- Arguments ----!
          real(kind=cp), dimension(:), intent(in)   :: x
          real(kind=cp), dimension(:), intent(out)  :: y
          real(kind=cp),               intent(in)   :: Twoth0
          real(kind=cp),               intent(in)   :: eta
          real(kind=cp),               intent(in)   :: Fwhm
          real(kind=cp),               intent(in)   :: asym1
          real(kind=cp),               intent(in)   :: asym2
       End Subroutine Calc_Pseudo_Voigt

       Pure Module Function Exponential(X,Par) Result (Ex_Val)
          !---- Arguments ----!
          real(kind=cp),              intent(in) :: x
          real(kind=cp), dimension(:),intent(in) :: par
          real(kind=cp)                          :: ex_val
       End Function Exponential

       Pure Module Subroutine Exponential_Der(X,Par,Ex_Val,Dpar)
          !---- Arguments ----!
          real(kind=cp),                        intent(in) :: x
          real(kind=cp),           dimension(:),intent(in) :: par
          real(kind=cp),                        intent(out):: ex_val
          real(kind=cp), optional, dimension(:),intent(out):: dpar
       End Subroutine Exponential_Der

       Pure Module Function Hat(X,Par) Result (H_Val)
          !---- Arguments ----!
          real(kind=cp),              intent(in) :: x
          real(kind=cp), dimension(:),intent(in) :: par
          real(kind=cp)                          :: h_val
       End Function Hat

       Pure Module Subroutine Hat_Der(X,Par,H_Val,Dpar)
          !---- Arguments ----!
          real(kind=cp),                        intent(in)  :: x
          real(kind=cp),           dimension(:),intent(in)  :: par
          real(kind=cp),                        intent(out) :: h_val
          real(kind=cp), optional, dimension(:),intent(out) :: dpar
       End Subroutine Hat_Der

       Pure Module Function Ikeda_Carpenter(X,Par) Result (Ik_Val)
          !---- Arguments ---!
          real(kind=cp),              intent(in) :: x
          real(kind=cp), dimension(:),intent(in) :: par
          real(kind=cp)                          :: ik_val
       End Function Ikeda_Carpenter

       Pure Module Subroutine Ikeda_Carpenter_Der(X,Par,Ik_Val,Dpar)
          !---- Arguments ----!
          real(kind=cp),                        intent(in)  :: x
          real(kind=cp),           dimension(:),intent(in)  :: par
          real(kind=cp),                        intent(out) :: ik_val
          real(kind=cp), optional, dimension(:),intent(out) :: dpar
       End Subroutine Ikeda_Carpenter_Der

       Pure Module Function Pseudovoigt(X,Par) Result (Pv_Val)
          !---- Arguments ----!
          real(kind=cp),              intent(in) :: x
          real(kind=cp), dimension(:),intent(in) :: par
          real(kind=cp)                          :: pv_val
       End Function Pseudovoigt

       Pure Module Subroutine Pseudovoigt_Der(X,Par,Pv_Val,Dpar)
          !---- Arguments ----!
          real(kind=cp),                       intent(in) :: x
          real(kind=cp), dimension(:),         intent(in) :: par
          real(kind=cp),                       intent(out):: pv_val
          real(kind=cp), optional,dimension(:),intent(out):: dpar
       End Subroutine Pseudovoigt_Der

       Pure Module Function TCH_pVoigt(X,Par) Result (Pv_Val)
          !---- Arguments ----!
          real(kind=cp),              intent(in) :: x
          real(kind=cp), dimension(:),intent(in) :: par
          real(kind=cp)                          :: pv_val
       End Function TCH_pVoigt

       Pure Module Subroutine TCH_pVoigt_Der(X,Par,Pv_Val,dPar)
          !---- Arguments ----!
          real(kind=cp),                       intent(in) :: x
          real(kind=cp), dimension(:),         intent(in) :: par
          real(kind=cp),                       intent(out):: pv_val
          real(kind=cp), optional,dimension(:),intent(out):: dpar
       End Subroutine TCH_pVoigt_Der

       Pure Module Subroutine Get_HG_HL(fwhm,eta,HG,HL)
         real(kind=cp), intent(in)  :: fwhm, eta
         real(kind=cp), intent(out) :: HG,HL
       End Subroutine Get_HG_HL

       Pure Module Subroutine Get_FWHM_Eta(HG,HL,fwhm,eta)
         real(kind=cp), intent(in)  :: HG
         real(kind=cp), intent(in)  :: HL
         real(kind=cp), intent(out) :: fwhm
         real(kind=cp), intent(out) :: eta
       End Subroutine Get_FWHM_Eta

       Pure Module Subroutine Keijser(h,eta,hg,hl)
          real(kind=cp), intent( in) :: h,eta
          real(kind=cp), intent(out) :: hg,hl
       End Subroutine Keijser

       Pure Module Function Split_Pseudovoigt(X,Par) Result (Pv_Val)
          !---- Arguments ----!
          real(kind=cp),              intent(in) :: x
          real(kind=cp), dimension(:),intent(in) :: par
          real(kind=cp)                          :: pv_val
       End Function Split_Pseudovoigt

       Pure Module Subroutine Split_Pseudovoigt_Der(X,Par,Pv_Val,Dpar)
          !---- Arguments ----!
          real(kind=cp),                       intent(in) :: x
          real(kind=cp), dimension(:),         intent(in) :: par
          real(kind=cp),                       intent(out):: pv_val
          real(kind=cp), optional,dimension(:),intent(out):: dpar
       End Subroutine Split_Pseudovoigt_Der

       Module Subroutine PsVoigtian(Twoth , Twoth0 , Eta , Gamm, Dprdt , Dprdg , Dprde, PsVoigt )
          !---- Arguments ----!
          real(kind=cp), intent(in)  :: twoth
          real(kind=cp), intent(in)  :: twoth0
          real(kind=cp), intent(in)  :: eta
          real(kind=cp), intent(in)  :: gamm
          real(kind=cp), intent(out) :: dprdt
          real(kind=cp), intent(out) :: dprdg
          real(kind=cp), intent(out) :: dprde
          real(kind=cp), intent(out) :: psvoigt
        End Subroutine PsVoigtian

       Pure Module Function Dfunc_Int(twopsi, twoth0) result(dfunc)
          !---- Arguments ----!
          Real(kind=cp), Intent(In)  :: twopsi
          Real(kind=cp), Intent(In)  :: twoth0
          Real(kind=cp)              :: dfunc
       End Function Dfunc_Int

       Pure Module Function Extra_Int(X) result(extra)
          !---- Arguments ----!
          Real(kind=cp), Intent(In) :: x
          Real(kind=cp)             :: extra
       End Function Extra_Int

       Module Subroutine Init_Prof_Val()
          !---- Arguments ----!
       End Subroutine Init_Prof_Val

       Module Subroutine Prof_Val( eta, gamm, asym1, asym2, twoth, twoth0, dprdt, dprdg,  &
                                   dprde , dprds , dprdd , profval, use_asym, use_hps)
          !---- Arguments ----!
          real(kind=cp),   Intent(In)    :: eta
          real(kind=cp),   Intent(In)    :: gamm
          real(kind=cp),   Intent(In)    :: asym1
          real(kind=cp),   Intent(In)    :: asym2
          real(kind=cp),   Intent(In)    :: twoth
          real(kind=cp),   Intent(In)    :: twoth0
          real(kind=cp),   Intent(Out)   :: dprdt
          real(kind=cp),   Intent(Out)   :: dprdg
          real(kind=cp),   Intent(Out)   :: dprde
          real(kind=cp),   Intent(Out)   :: dprds
          real(kind=cp),   Intent(Out)   :: dprdd
          real(kind=cp),   Intent(Out)   :: profval
          Logical,         Intent(In)    :: use_asym
          Logical,         Intent(In)    :: use_hps
       End Subroutine Prof_Val

       Pure Module Function Gaussian(X,Par) Result (Gauss_Val)
          !---- Arguments ----!
          real(kind=cp),              intent(in) :: x
          real(kind=cp), dimension(:),intent(in) :: par
          real(kind=cp)                          :: gauss_val
       End Function Gaussian

       Pure Module Subroutine Gaussian_Der(X,Par,Gauss_Val,Dpar)
          !---- Arguments ----!
          real(kind=cp),                       intent(in) :: x
          real(kind=cp),          dimension(:),intent(in) :: par
          real(kind=cp),                       intent(out):: gauss_val
          real(kind=cp), optional,dimension(:),intent(out):: dpar
       End Subroutine Gaussian_Der

       Module Subroutine Prof_Gaussian(Pos , Pos0 , Gamm , Dgdt , Dgdg, Gauss )
          !---- Arguments ----!
          real(kind=cp), intent(in)  :: pos
          real(kind=cp), intent(in)  :: pos0
          real(kind=cp), intent(in)  :: gamm
          real(kind=cp), intent(out) :: dgdt
          real(kind=cp), intent(out) :: dgdg
          real(kind=cp), intent(out) :: gauss
       End Subroutine Prof_Gaussian

       Pure Module Function Lorentzian(X,Par) Result (Lor_Val)
          !---- Arguments ----!
          real(kind=cp),              intent(in) :: x
          real(kind=cp), dimension(:),intent(in) :: par
          real(kind=cp)                          :: lor_val
       End Function Lorentzian

       Module Subroutine Prof_Lorentzian(Pos , Pos0 , Gamm, Dldt , Dldg, Lorentz )
          !---- Arguments ----!
          real(kind=cp), intent(in) :: pos
          real(kind=cp), intent(in) :: pos0
          real(kind=cp), intent(in) :: gamm
          real(kind=cp), intent(out):: dldt
          real(kind=cp), intent(out):: dldg
          real(kind=cp), intent(out):: lorentz
       End Subroutine Prof_Lorentzian

       Pure Module Subroutine Lorentzian_Der(X,Par,Lor_Val,Dpar)
          !---- Arguments ----!
          real(kind=cp),                        intent(in) :: x
          real(kind=cp),           dimension(:),intent(in) :: par
          real(kind=cp),                        intent(out):: lor_val
          real(kind=cp), optional, dimension(:),intent(out):: dpar
       End Subroutine Lorentzian_Der

       Module Subroutine Tof_Carpenter(Dt,D,Alfa,Beta,Gamm,Eta,Kappa,Tof_Theta,Tof_Peak,Deriv)
          !---- Arguments ----!
          real(kind=cp),             intent( in) :: dt
          real(kind=cp),             intent( in) :: d
          real(kind=cp),             intent( in) :: alfa
          real(kind=cp),             intent( in) :: beta
          real(kind=cp),             intent( in) :: gamm
          real(kind=cp),             intent( in) :: eta
          real(kind=cp),             intent( in) :: kappa
          real(kind=cp),             intent( in) :: tof_theta
          real(kind=cp),             intent(out) :: tof_peak
          type(Deriv_TOF_Type), optional, intent(out) :: deriv
       End Subroutine Tof_Carpenter

       Module Subroutine Tof_Jorgensen(Dt,Alfa,Beta,Sigma,Tof_Peak,Deriv)
          !---- Arguments ----!
          real(kind=cp),                  intent( in)  :: dt
          real(kind=cp),                  intent( in)  :: alfa
          real(kind=cp),                  intent( in)  :: beta
          real(kind=cp),                  intent( in)  :: sigma
          real(kind=cp),                  intent(out)  :: tof_peak
          type(Deriv_TOF_Type), optional, intent(out)  :: deriv
       End Subroutine Tof_Jorgensen

       Module Subroutine Tof_Jorgensen_Vondreele(Dt,Alfa,Beta,Gamm,Eta,Tof_Peak,Deriv)
          !---- Arguments ----!
          real(kind=cp),             intent( in) :: dt
          real(kind=cp),             intent( in) :: alfa
          real(kind=cp),             intent( in) :: beta
          real(kind=cp),             intent( in) :: gamm
          real(kind=cp),             intent( in) :: eta
          real(kind=cp),             intent(out) :: tof_peak
          type(Deriv_TOF_Type), optional, intent(out) :: deriv
       End Subroutine Tof_Jorgensen_Vondreele

    End Interface

 Contains

   !!--++
   !!--++ Function Expi_e1(z) Result(Exp_e1)
   !!--++    complex(kind=dp), intent (in) ::  z
   !!--++    complex(kind=dp)              ::  exp_e1
   !!--++
   !!--++    Exponential integral function
   !!--++
   !!--++ Update: October - 2005
   !!
   Function Expi_E1(Z)  Result(Exp_E1)
      !---- Argument ----!
      complex(kind=dp), intent (in) ::  z
      complex(kind=dp)              ::  exp_e1

      !---- Local variables ----!
      integer                  :: k
      real(kind=dp), parameter :: el=0.5772156649015328_dp
      real(kind=dp)            :: a0,x,km
      complex(kind=dp)         :: cr,ct0

      x=real(z)
      a0=abs(z)
      if (a0 <= 0.0_dp) then
         exp_e1=(1.0e+300_dp,0.0_dp)
      else if (a0 <= 10.0_dp .or. x <= 0.0_dp .and. a0 < 20.0_dp) then
         exp_e1=(1.0_dp,0.0_dp)
         cr=(1.0_dp,0.0_dp)
         do k=1,150
            km=real(k,kind=dp)
            cr=-cr*km*z/(km+1.0_dp)**2
            exp_e1=exp_e1+cr
            if ( abs(cr) <=  abs(exp_e1)*1.0e-15_dp) exit
         end do
         exp_e1=(-el- log(z)+z*exp_e1)*exp(z)
      else
         ct0=(0.0_dp,0.0_dp)
         do k=120,1,-1
            km=real(k,kind=dp)
            ct0=km/(1.0_dp+km/(z+ct0))
         end do
         exp_e1=1.0_dp/(z+ct0)
         if (x <= 0.0_dp .and. abs(aimag(z)) <= 1.0e-9_dp)  &
                         exp_e1=exp_e1-pi*(0.0_dp,1.0_dp)*exp(z)
      end if

   End Function Expi_E1

End Module  CFML_Profiles
