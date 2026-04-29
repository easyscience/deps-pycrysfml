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
!!---- MODULE: CFML_Random_Generators
!!----   INFO: A module for random number generation for differents
!!----         distributions.
!!----
!!----
!!--..    Distribution                    Function/subroutine name
!!--..    --------------------------------------------------------
!!--..    Normal (Gaussian)               random_normal
!!--..    Gamma                           random_gamma
!!--..    Chi-squared                     random_chisq
!!--..    Exponential                     random_exponential
!!--..    Weibull                         random_Weibull
!!--..    Beta                            random_beta
!!--..    t                               random_t
!!--..    Multivariate normal             random_mvnorm
!!--..    Generalized inverse Gaussian    random_inv_gauss
!!--..    Poisson                         random_Poisson
!!--..    Binomial                        random_binomial1   *
!!--..                                    random_binomial2   *
!!--..    Negative binomial               random_neg_binomial
!!--..    von Mises                       random_von_Mises
!!--..    Cauchy                          random_Cauchy
!!--..
!!--..    Generate a random ordering of the integers 1 .. N
!!--..    random_order
!!--..
!!--..    Initialize (seed) the uniform random number generator
!!--..    for ANY compiler seed_random_number
!!--..
!!--..    Two functions are provided for the binomial distribution.
!!--..    If the parameter values remain constant, it is recommended that the
!!--..    first subroutine is used (random_binomial1).   If one or both of the
!!--..    parameters change, use the second subroutine (random_binomial2).
!!--..
!!--..    The compilers own random number generator,
!!--..    SUBROUTINE RANDOM_NUMBER(r), is used to provide a source of
!!--..    uniformly distributed random numbers.
!!--..
!!--..    At this stage, only one random number is generated at each call to
!!--..    one of the functions above.
!!--..
!!--..    The module uses the following functions which are included here:
!!--..    bin_prob to calculate a single binomial probability lngamma
!!--..    to calculate the logarithm to base e of the gamma subroutine
!!--..
!!--..    Some of the code is adapted from Dagpunar"s book:
!!--..        Dagpunar, J. "Principles of random variate generation"
!!--..        Clarendon Press, Oxford, 1988.   ISBN 0-19-852202-9
!!--..
!!--..    In most of Dagpunar"s routines, there is a test to see whether
!!--..    the value of one or two floating-point parameters has changed
!!--..    since the last call.
!!--..    These tests have been replaced by using a logical variable FIRST.
!!--..    This should be set to .TRUE. on the first call using new values
!!--..    of the parameters, and .FALSE. if the parameter values are the
!!--..    same as for the previous call.
!!--..
!!--..    Version 1.11, 4 January 1999
!!--..    Changes from version 1.01
!!--..    1. The random_order, random_Poisson & random_binomial routines
!!--..       have been replaced with more efficient routines.
!!--..    2. A routine, seed_random_number, has been added to seed the
!!--..       uniform random number generator. This requires input of the
!!--..       required number of seeds for the particular compiler from a
!!--..       specified I/O unit such as a keyboard.
!!--..    3. Made compatible with Lahey"s ELF90.
!!--..
!!--..       Author: Alan Miller
!!--..       CSIRO Division of Mathematical & Information Sciences
!!--..       Private Bag 10, Clayton South MDC
!!--..       Clayton 3169, Victoria, Australia
!!--..       Phone: (+61) 3 9545-8036      Fax: (+61) 3 9545-8080
!!--..       e-mail: Alan.Miller @ vic.cmis.csiro.au

!!----
!!
 Module CFML_Random
    !---- Use Modules ----!
    Use CFML_GlobalDeps, only: cp, dp, Err_CFML, clear_error

    !---- Local Variables ----!
    implicit none

    private

    !---- Public Procedures ----!
    public  :: random_beta, random_binomial1, random_binomial2,  &
               random_cauchy, random_chisq,                      &
               random_exponential,                               &
               random_gamma, random_gamma1, random_gamma2,       &
               random_inv_gauss,                                 &
               random_mvnorm,                                    &
               random_neg_binomial, random_normal,               &
               random_order,                                     &
               random_poisson,                                   &
               random_t,                                         &
               random_von_mises,                                 &
               random_weibull,                                   &
               seed_random_number

    !--------------------!
    !---- PARAMETERS ----!
    !--------------------!
    real(kind=cp), parameter :: HALF = 0.5_cp
    real(kind=cp), parameter :: ONE  = 1.0_cp
    real(kind=cp), parameter :: TWO  = 2.0_cp
    real(kind=cp), parameter :: ZERO = 0.0_cp
    real(kind=cp), parameter :: VLARGE = huge(1.0_cp)
    real(kind=cp), parameter :: VSMALL = tiny(1.0_cp)

    Interface
       Module Function Bin_Prob(N, P, R) Result(Fn_Val)
          !---- Arguments ----!
          integer, intent(in)         :: n, r
          real(kind=cp), intent(in)   :: p
          real(kind=cp)               :: fn_val
       End Function Bin_Prob

       Module Function Gg_F(Methodeg) Result(Gg)
          !---- Arguments ----!
          integer,       intent(in) :: methodeg
          real(kind=cp)             :: gg
       End Function Gg_F

       Module Function Gpg_F(Mt,Methodeg,Gpstab) Result(Gpg)
          !---- Arguments ----!
          real(kind=cp), intent(in) :: mt
          integer,       intent(in) :: methodeg,gpstab
          integer                   :: gpg
       End Function Gpg_F

       Module Function Gpp_F(Mt) Result(Gpp)
          !---- Arguments ----!
          real(kind=cp), intent(in) :: mt
          integer                   :: gpp
       End Function Gpp_F

       Module Function Integral(A, B, Dk) Result(Resultt)
          !---- Arguments ----!
          real(kind=cp), intent(in)      :: a, b
          real(kind=dp), intent(in)      :: dk
          real(kind=cp)                  :: resultt
       End Function Integral

       Module Function Lngamma(X) Result(Fn_Val)
          !---- Arguments ----!
          real(kind=dp), intent(in) :: x
          real(kind=dp)             :: fn_val
       End Function Lngamma

       Module Function Random_Beta(Aa, Bb, First) Result(Fn_Val)
          !---- Arguments ----!
          real(kind=cp), intent(in)    :: aa, bb
          logical,       intent(in)    :: first
          real(kind=cp)                :: fn_val
       End Function Random_Beta

       Module Function Random_Binomial1(N, P, First) Result(Ival)
          !---- Arguments ----!
          integer,       intent(in)  :: n
          real(kind=cp), intent(in)  :: p
          logical,       intent(in)  :: first
          integer                    :: ival
       End Function Random_Binomial1

       Module Function Random_Binomial2(N, Pp, First) Result(Ival)
          !---- Arguments ----!
          integer,       intent(in)    :: n
          real(kind=cp), intent(in)    :: pp
          logical,       intent(in)    :: first
          integer                      :: ival
       End Function Random_Binomial2

       Module Function Random_Cauchy() Result(Fn_Val)
          !---- Arguments ----!
          real(kind=cp) :: fn_val
       End Function Random_Cauchy

       Module Function Random_ChiSQ(Ndf, First) Result(Fn_Val)
          !---- Arguments ----!
          integer, intent(in) :: ndf
          logical, intent(in) :: first
          real(kind=cp)       :: fn_val
       End Function Random_ChiSQ

       Module Function Random_Exponential() Result(Fn_Val)
          !---- Arguments ----!
          real(kind=cp)  :: fn_val
       End Function Random_Exponential

       Module Function Random_Gamma(S, First) Result(Fn_Val)
          !---- Arguments ----!
          real(kind=cp), intent(in)  :: s        !shape parameter of distribution (0.0 < real)
          logical,       intent(in)  :: first
          real(kind=cp)              :: fn_val
       End Function Random_Gamma

       Module Function Random_Gamma1(S, First) Result(Fn_Val)
          !---- Arguments ----!
          real(kind=cp), intent(in)   :: s         ! shape parameter of distribution (1.0 < real)
          logical,       intent(in)   :: first
          real(kind=cp)               :: fn_val
       End Function Random_Gamma1

       Module Function Random_Gamma2(S, First) Result(Fn_Val)
          !---- Arguments ----!
          real(kind=cp), intent(in)   :: s         ! shape parameter of distribution (1.0 < real)
          logical,       intent(in)   :: first
          real(kind=cp)               :: fn_val
       End Function Random_Gamma2

       Module Function Random_Inv_Gauss(H, B, First) Result(Fn_Val)
          !---- Arguments ----!
          real(kind=cp), intent(in)  :: h, b      ! parameter of distribution (0 <= real)
          logical,       intent(in)  :: first
          real(kind=cp)              :: fn_val
       End Function Random_Inv_Gauss

       Module Function Random_Neg_Binomial(Sk, P) Result(Ival)
          !---- Arguments ----!
          real(kind=cp), intent(in)   :: sk
          real(kind=cp), intent(in)   :: p
          integer                     :: ival
       End Function Random_Neg_Binomial

       Module Function Random_Normal() Result(Fn_Val)
          !---- Arguments ----!
          real(kind=cp) :: fn_val
       End Function Random_Normal

       Module Subroutine Random_Mvnorm(N, H, D, First, F, X)
          !---- Arguments ----!
          integer,                            intent(in) :: n
          real(kind=cp), dimension(n),        intent(in) :: h
          real(kind=cp), dimension(n*(n+1)/2),intent(in) :: d
          logical,                            intent(in) :: first
          real(kind=cp), dimension(n*(n+1)/2),intent(out):: f
          real(kind=cp), dimension(n),        intent(out):: x
       End Subroutine Random_Mvnorm

       Module Function Random_Poisson(mt) Result(Ival)
          !---- Arguments ----!
          real(kind=cp), intent(in)    :: mt
          integer                      :: Ival
       End Function Random_Poisson

       Module Function Random_T(M) Result(Fn_Val)
          !---- Arguments ----!
          integer,      intent(in)  :: m
          real(kind=cp)             :: fn_val
       End Function Random_T

       Module Function Random_Von_Mises(K, First) Result(Fn_Val)
          !---- Arguments ----!
          real(kind=cp), intent(in)  :: k
          logical,       intent(in)  :: first
          real(kind=cp)              :: fn_val
       End Function Random_Von_Mises

       Module Function Random_Weibull(a) Result(Fn_Val)
          !---- Arguments ----!
          real(kind=cp), intent(in)  :: a
          real(kind=cp)              :: fn_val
       End Function Random_Weibull


    End Interface

 Contains

    !!----
    !!---- RANDOM_ORDER
    !!----
    !!----    Generate a random ordering of the integers 1 ... n.
    !!----
    !!---- 14/04/2019
    !!
    Subroutine Random_Order(N, Order)
       !---- Arguments ----!
       integer,              intent(in)  :: n
       integer, dimension(n),intent(out) :: order

       !---- Local variables ----!
       integer       :: i, j, k
       real(kind=cp) :: wk

       do i = 1, n
          order(i) = i
       end do

       !---- starting at the end, swap the current last indicator with one
       !---- randomly chosen from those preceeding it.
       do i = n, 2, -1
          call random_number(wk)
          j = 1 + i * wk
          if (j < i) then
             k = order(i)
             order(i) = order(j)
             order(j) = k
          end if
       end do

    End Subroutine Random_Order

    !!----
    !!---- SEED_RANDOM_NUMBER
    !!----
    !!---- 14/04/2019
    !!
    Subroutine Seed_Random_Number(I_input,I_output)
       !---- Arguments ----!
       integer, optional, intent(in)  :: I_input
       integer, optional, intent(in)  :: I_output

       !---- Local variables ----!
       integer                            :: k,lun1,lun2
       integer, dimension(:), allocatable :: seed

       call random_seed(size=k)
       allocate( seed(k) )

       lun1=5
       lun2=6
       if (present(i_input))  lun1=i_input
       if (present(i_output)) lun2=i_output

       write(unit=*, fmt= "(a, i2, a)")" Enter ", k, " integers for random no. seeds: "
       read(unit=lun1, fmt=*) seed

       write(unit=lun2,fmt="(a, (7i10))") " Random no. seeds: ", seed
       call random_seed(put=seed)

    End Subroutine Seed_Random_Number

 End Module CFML_Random
