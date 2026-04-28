!!----  Program MCMAG, written by Philippe Lacorre in 1988
!!----  Adapted to Fortran 90 and some more modifications by J.Rodriguez-Carvajal
!!----  A new input file containing the whole set of instructions for running
!!----  the program in batch mode has been implemented. Output for FullProf Studio
!!----  and for calculating the single crystal and powder neutron diffraction patterns
!!----  has also been implemented (May 2015, JRC)
!!----  This file contains all the program, there is no dependence on CrysFML
!!----  Few functions (u_case,l_case, invert) have been imported inside the module
!!----  mcm_inc to facilitate reading of the new input file.
!!----
!!----
!                  M   M   CCC  M   M   AA    GGG
!                  MM MM  C     MM MM  A  A  G
!                  M M M  C     M M M  AAAA  G GGG
!                  M   M  C     M   M  A  A  G   G
!  *************** M   M   CCC  M   M  A  A   GGG ****************
! *                                                               *
! *   MCMAG - A FORTRAN PROGRAM TO SIMULATE MAGNETIC STRUCTURES   *
! *                  Version May 2015 (JRC-ILL)                   *
! *                                                               *
! *                    written by P. Lacorre                      *
! *                modified by J. Rodriguez-Carvajal              *
! *  Institut Laue-Langevin, 156X, 38042 Grenoble Cedex - FRANCE  *
! *                             and                               *
! *  Laboratoire des Fluorures URA CNRS 449, Faculte des Sciences *
! *         Universite du Maine, Avenue Olivier-Messiaen          *
! *                 72017 Le Mans Cedex - FRANCE                  *
! *                                                               *
! *****************************************************************

!      MCMAG is a Fortran program to simulate magnetic models from
!  postulated coupling constants. Ground state configurations are
!  obtained by a simulated annealing algorithm [S. Kirkpatrick,
!  C.D. Gellatt Jr. and M.P. Vecchi, Science, 220 (1983) 671] and
!  the configuration space is explored with random walks by using
!  Metropolis importance sampling [N. Metropolis, A.W. Rosenbluth,
!  M.N. Rosenbluth, A.H. Teller and E. Teller, J. Chem. Phys. 21
!  (1953) 1087]. Any kind of 1D, 2D or 3D periodic model, magnetic
!  cluster as well as real magnetic structure can be simulated
!  without limitation of interacting distance.

!  Further details on the method used for this purpose are given
!  in the paper :
!  MCMAG: a computer program to simulate magnetic structures.
!  P. Lacorre and J. Pannetier, J. Magn. Magn. Mat. 71 (1987) 63.

!  Topological and magnetic parameters must be stored in a file
!  MYFILE.mcm (where MYFILE is a filename given by user)
!  This input file contains a list of the magnetic
!  sites of the structure, together with a list of their neighbours
!  and corresponding exchange integrals, as well as anisotropy
!  coefficients and spin amplitudes (for a complete description see
!  the manual MCMAG.MAN). No symmetry element is taken into account
!  by the program: all spins are independent.
!  The so-called basic unit cell (b.u.c.) which is described in
!  the input file represents the smallest sample that can be used
!  for a simulation. Larger size samples are built up from the
!  stacking of such unit cells along the three crystallographic
!  axes and defined by the number of cells along these axes (namely
!  LU, LV and LW for axes U, V and W respectively). Three kinds of
!  boundary conditions are allowed:
!       - free edges
!       - periodic boundary conditions
!       - mixed conditions (<=> free edges along W and periodic
!         boundary conditions along U and V)

!  The upper limit in sample size is imposed by the array dimension
!  of the program. In order to allow an easy modification of these
!  limits (which could be convenient for the study of specific cases)
!  they are introduced as parameters in the module mcm_inc
!  These upper limits are called :
!       - NUMAX : maximal number of cells along the U axis
!       - NVMAX : maximal number of cells along the V axis
!       - NWMAX : maximal number of cells along the W axis
!       - NAMAX : maximal number of sites per cell
!       - NNMAX : maximal number of neighbours per site
!
!  The initial configuration of spins is either randomly generated
!  by the program or can be imposed (file MYFILE.ini).
!  During the simulation, new orientations of spins are drawn at
!  random and adopted or rejected (according to Boltzmann statistics)
!  in order to progressively minimize the energy of the system.
!  The Hamiltonian used for the energy calculation includes three
!  terms :
!       - a general two-spin coupling term with facility for anisotropic
!         asymmetric exchange. General tensor [Jij]
!       - a single-ion anisotropy term    Di(ui.Si)^2 (ui is a unitary vector)
!       - an applied magnetic field term  H
!  The energy formula per site i may be expressed as follows:
!  (with the convention that a vector V' represents the transposed
!  form of a column vector V)

!       Ei = - 0.5*Sum(j){Si'[Jij]Sj} + D(ui'Si)**2 - H'Si

!       where :
!          Si,Sj  are vectors representing two interacting spins
!          [Jij]  is the coupling tensor:
!                                                         Jxx Jxy Jxz
!                 for every <i,j> pair, [Jij] represents  Jyx Jyy Jyz
!                                                         Jzx Jzy Jzz
!          Di     is the strength of the single-ion anisotropy for site i
!          ui     is a unitary vector indicating the direction of the anisotropy
!          H      is the vector of applied magnetic field
!          Sum(j) is the sum over all neighbours (j) of site i

!  The reference used for spins is totally independent of the crys-
!  tallographic one: spins are always generated and expressed in an
!  orthonormal coordinate system during the simulation process. The
!  same orthonormal coordinate system is used for the anisotropy
!  coefficients and for the applied magnetic field ifany. For sake
!  of convenience, a possibility is offered at the end of the
!  simulation, to project the magnetic moments on the real crystal
!  axes and to rotate the whole system to align one moment along a
!  desired direction of the crystal frame of reference. This option
!  facilitates the comparison between the result of simulation and
!  real or theoretical arrangements of magnetic moments.

!  The external conditions of the simulation (cooling rate, varying
!  field ...) are introduced in an interactive way, as sequences of
!  iteration on temperature at H=cst or on magnetic field at T=cst.
!  The arrangements of magnetic moments and last spin configuration
!  are stored during the process and at the end of simulation, respec-
!  tively. Averaged characteristics such as magnetic susceptibility
!  or specific heat and also energy and magnetization are collected
!  at every step of the iterations.

!  The following files are generated during or at the end of the
!  simulation :

!  - MYFILE.red : contains the sequence of instructions given by
!                 user during the run of MCMAG (can be used as ins-
!                 truction file for further batch runs)

!  - MYFILE.lis : contains all informations about the arrangement of
!                 magnetic moments and the averaged characteristics
!                 collected at every step of the simulation process

!  - MYFILE.res : created at user's request, this file stores averaged
!                 characteristics collected at every temperature or
!                 field step, for further analysis or plotting

!  - MYFILE.spi : created at the end of simulation, it contains the
!                 last generated spin configuration of the simulation.
!                 The file format is the same as that of file MYFILE.INI,
!                 which allows to split up a simulation in several runs

!  - MYFILE.fst : created at the end of simulation for visualizing the
!                 last generated spin configuration of the simulation.

!  - MYFILE.cfl : created at the end of simulation for visualizing the
!                 last average spin configuration of the simulation, or
!                 for calculating the magnetic diffraction pattern.

!       The main program calls the following Subroutines :

!  read_FILE   : read input data and simulation parameters
!  COMP_TEST   : check internal consistency of data
!  MOD_INT     : ifactivated, modify some selected isotropic coupling
!  GEN_SPI     : generate or read initial spin configuration
!  MC_CYCLE    : execute the Monte Carlo loops
!  EN_TOT      : compute the total energy of the system
!  CONST_FUNCT : compute the constraint function (isotropic coupling)
!  OUT_RES     : output averaged characteristics
!  OUT_CONF    : output the configuration of magnetic moments
!  PRO_ROTA    : ifselected, project moments on crystal cell (rotate)

!  These Subroutines call other Subroutines or function :

!  BOUND_COND : compute the selected boundary conditions
!  ENn_CALC   : compute energy difference between new and old spin conf.
!  RANnD      : generate a pseudo-random vector
!  RANF       : generate a pseudo-random number between 0 and 1

!  Four Subroutines ENn_CALC and four Subroutines RANnD are included in
!  the program (n = 1, 2, 3 or q). Only one among the four is activated
!  during a simulation, depending on the selected spin model, namely :
!  - Ising spin model           (n = 1)
!  - XY spin model              (n = 2)
!  - Heisenberg spin model      (n = 3)
!  - q-states planar Potts model(n = q)

!***********************************************************************
!  The following lines give a list of the main parameters and arrays
!  used in MCMAG (main program and Subroutines) :
!    NAV                   : indices of the neighbours of a site
!    PT                    : pointer of NAV
!    NU,NV,NW              : absolute indices of neighbouring cells
!    NAT                   : indices of sites in a cell
!    NVS                   : total number of neighbours of a site
!    M                     : relative indices of neighbouring cells
!    AA,AD                 : matrices converting between Cartesian and Crytallographic frames
!    TCO                   : test of internal compatibility
!                            (topological self-consistency)
!    TCOJX,JY,JZ,JXY,JXZ   : test of internal compatibility (interactions)
!    ...JYX,JYZ,JZX,JZY
!    AMPS,AMPS2            : spin amplitude
!    SX,SY,SZ              : spin components
!    SOMX,SOMY,SOMZ        : components of magnetic moment
!    COMX                  : storage of SOMX when SOMX is used for
!                            another purpose
!    TMOM                  : total magnetic moment
!    SOMRX,SOMRY,SOMRZ     : components of magnetic moment after rotation
!    SOMX2,SOMY2,SOMZ2     : variance and standard deviation of magnetic
!                            moment
!    SOMRX2,SOMRY2,SOMRZ2  : variance and standard deviation of magnetic
!        moment after rotation
!    ISIGX,ISIGY,ISIGZ     : 1000*(standard deviation of components)
!    ISIGM                 : 1000*(standard deviation of total moment)
!    AJSX,AJSY,AJSZ        : components of the local molecular field
!    AJXX,AJXY,AJXZ        : elements of the exchange tensor
!    AJYX,AJYY,AJYZ
!    AJZX,AJZY,AJZZ
!    AJJX,AJJY,AJJZ        : diagonal elements of the exchange tensor
!                            or storage of isotropic (AJXX,AJYY,AJZZ)
!                            before eventual modifications
!    D                     : anisotropy coefficient
!    DDIX,DDIY,DDIZ        : direction of anisotropy
!    D0                    : normalizing factor for the direction of
!                            anisotropy
!    DX,DY,DZ              : normalized direction of anisotropy
!===========================================================================

 Module mcm_inc
    !Global variables of the program MCMAG (no more COMMONs!)
    implicit none
    integer, parameter, public :: sp = selected_real_kind(6,30)
    integer, parameter, public :: dp = selected_real_kind(14,150)
    real(kind=dp), parameter, public ::  pi = 3.141592653589793238463_dp
    integer, parameter, public ::    &
              numax=30,    &    ! maximal number of cells along the U axis
              nvmax=30,    &    ! maximal number of cells along the V axis
              nwmax=30,    &    ! maximal number of cells along the W axis
              NAMAX=100,   &    ! maximal number of sites per cell
              NNMAX=50,    &    ! maximal number of neighbours per site
              NMOMAX=200,  &    ! maximal number of isotropic coupling
                                !   constants which can be modified
                                !   interactively
              nal=8,       &    ! maximal number of site moments to be
                                !   written on a single line of the output
                                !   file MYFILE.LIS
              I01=31,      &    !  input unit number for file MYFILE.DAT
              I02=32,      &    !  input unit number for file MYFILE.INI
              I03=33,      &    ! output unit number for file MYFILE.RES
              I04=34,      &    ! output unit number for file MYFILE.SPI
              IBA=35,      &    ! output unit number for file MYFILE.REC
              LPT=7,       &    ! output unit number for file MYFILE.LIS
              ITTI=5,      &    ! input unit number for terminal
                                !   (interactive mode)
              ITTO=6            ! output unit number for terminal
                                !   (interactive mode)

          real(kind=sp), dimension(namax)                         :: d,ddix,ddiy,ddiz, dx,dy,dz, d0, amps, amps2, &
                                                                     fjsx,fjsy,fjsz,fjsm
          character(len=6), dimension(namax) :: namt=" ",scattf=" "
          character(len=2), dimension(namax) :: elem=" "
          real(kind=sp), dimension(3,namax)  :: xyz=0.0
          logical                            :: coordinates_read=.false.,rotated=.false.
          logical                            :: batch=.false.
          real(kind=sp), dimension(namax,numax+1,nvmax+1,nwmax+1) :: sx, sy, sz
          integer,       dimension(namax,nnmax,3)                 :: M !Connectivity matrix with relative translations
          integer,       dimension(namax,nnmax)                   :: nav !
          integer,       dimension(namax)                         :: isigx,isigy,isigz,isigm,nu,nv,nw

          integer,       dimension(namax)                         :: Nvs, &!Total number of neighbours of a site
                                                                     nat !
          real(kind=sp), dimension(namax,nnmax)                   :: ajjx,ajjy,ajjz,ajxy,ajxz,ajyx,ajyz,ajzx,ajzy, &
                                                                     ajxx,ajyy,ajzz
          real(kind=sp), dimension(nnmax)                         :: pt
          real(kind=dp), dimension(3)                             :: tco

          real(kind=sp), dimension(namax)                         :: ajsx,ajsy,ajsz

          real(kind=sp),dimension(namax,numax,nvmax,nwmax)        :: somx, somx2,somy2,somz2, somrx,somry,somrz, &
                                                                     somrx2,somry2,somrz2
          real(kind=sp),dimension(namax,numax+1,nvmax+1,nwmax+1)  :: somy,somz,comx,tmom

          integer,       dimension(nmomax)  ::ium,ivm,iwm,nm,numb
          real(kind=sp), dimension(nmomax)  ::ajmod


          integer(kind=8) :: npaj ! Number of accepted jumps

          integer ::      lu,lv,lw,   &! Number of unit cells along u,v,w
                          it,         &! Number of Montecarlo cycles
                          nom,        &! Number of thermalization cycles
                          ianb,       &! Boundary condirions (1: periodic, 0:free, -1:mixed)
                          na,         &! Total number of sites in the basic unit cell (if negative no single ion anisotropy)
                          iani,       &! Index for single ion anisotropy (set to -1 ifna < 0, before puttin na=|na|)
                          iran,       &! Index for the spin-model (1:Ising, 2:planar XY, 3:Heisenberg, 4: q-state Potts planar)
                          jcod,       &! Code of interactions (0: isotropic, 1: anisotropic diagonal tensor, 2: anisotropic)
                          nmot,       &! Number of formula units per cell
                          nma,        &! Total number of unit cells: nma=lu*lv*lw
                          nq,         &! Number of q-states in q-Potts models
                          infa,nfa,   &! Option for averages (1:sample, 2:unit cell, 3: site, 4:mole)
                          nta,        &! Number of units for averages(1:sample, nma: cell, nta:site, nma*nmot: mole)
                          j1,j2,j3,   &! Indices for three sites (output)
                          last,       &! =1 for the last iteration
                          iwmx,iwmxy, &! Printing controls
                          nor,        &! Ordinal number of the moment to be re-oriented
                          im,iwrt=0,  &! Index, counter for cpu_time
                          ntemp,job    ! Total number of temperatures, job number

          real(kind=sp):: t,           &! Initial temperature
                        coef,          &! Kirpatrick coefficient (>1 heating, <1 cooling)
                        tfin,          &! Final temperature
                        hk,            &! Strength of magnetic field in kelvins
                        hx0,hy0,hz0,   &! Direction of magnetic field
                        hsta,hste,hfin,&! Initial, step and final magnetic field
                        hx,hy,hz,      &! Effective components of the magnetic field
                        hdix,hdiy,hdiz,&! Intermediate components of the magnetic field
                        snom,          &! snom=FLOAT(it-nom)
                        amx,amy,amz,cpu,cpu_par,remaining
        real(kind=sp) :: delta, & ! Current energy difference between the old and new configuration
                         x,y,z, & ! Randomly generated components of a spin
                         dorix,doriy,doriz,dorx,dory,dorz  !Direction to rotate a spin (X-tal and Cartesian systems)
        real(kind=sp) :: fconst,rc  !Constraint functions
        integer       :: nfip,  &! ifzero no random spin S=(x,y,z) is generated before calculating delta
                         nmult   ! Multiplier (2) applied to H and D in order to calculate the energy E1=1/2 E0

          character (len=132):: titl1,titl2,titl3,itit1,itit2,itit3
          character (len=50) :: titl
          character (len=10) :: bcond
          character (len=120):: nfil
          character (len=9)  :: avchar
          character (len=12) :: spimod
          character (len=1)  :: ansi,   &! Answer for correct file
                                ansmod, &! Answer for modifying locally the exchange interactions
                                ians,   &! R of I (Random initial configuration or read from input file)
                                ansnev, &! Printout of the configuration of magnetic moments (N: Never, E: end, A: All temperatures)
                                ansrf,  &! Output of averaged characteristics in file *.res
                                ansc,   &! Continue or not
                                ansh,   &! Interation of temperature (T) or Field (F)
                                cod,    &! Code for multiplier (M) or for additive coefficient (A)
                                ansm,   &! Printout of the coomponents of magnetic moments in crystallographic system
                                codt     ! Code '+' or 'x' depending on cod.
        real(kind=dp) :: tcojx,tcojy,tcojz, tcojxy,tcojxz,tcojyz,tcojyx,tcojzx,tcojzy !Tests of internal compatibility variables
        real(kind=dp) :: en,enm,en2m,mag2
        real(kind=dp), dimension(3,3) :: aa,ac,ad
        real(kind=sp) :: aax,aay,aaz,aaa,aab,aag !Cell parameters

  contains

      Subroutine conv_matrix()
        !  This Subroutine calls the following function:
        !     INVERT
         real(kind=dp) :: degre,aar
         degre=pi/180.0_dp
         aaa=aaa*degre
         aab=aab*degre
         aag=aag*degre

         aa(:,:)=0.0_dp
         ad(:,:)=0.0_dp

         aar=SQRT( 1.0-COS(aaa)*COS(aaa) -COS(aab)*COS(aab)  &
             -COS(aag)*COS(aag) +2*COS(aaa)*COS(aab)*COS(aag) )
         aa(1,1)=1.0
         aa(1,2)=COS(aag)
         aa(1,3)=COS(aab)
         aa(2,2)=SIN(aag)
         aa(2,3)=(COS(aab)*COS(aag)-COS(aaa))/SIN(aag)
         aa(3,3)=aar/SIN(aag)

         ad(1,1)=aax
         ad(1,2)=aay*COS(aag)
         ad(1,3)=aaz*COS(aab)
         ad(2,2)=aay*SIN(aag)
         ad(2,3)=aaz*(COS(aab)*COS(aag)-COS(aaa))/SIN(aag)
         ad(3,3)=aaz*aar/SIN(aag)

         aaa=aaa/degre
         aab=aab/degre
         aag=aag/degre
         !Calculates AC=INV(AA)
         ac=Invert(aa)
      end Subroutine conv_matrix

      Function Invert(a) Result(b)
         !---- Arguments ----!
         real(kind=dp),dimension(3,3), intent(in) :: a
         real(kind=dp),dimension(3,3)             :: b

         !---- Local variables ----!
         real(kind=dp)  :: dmat

         b(1,1) =   a(2,2)*a(3,3)-a(2,3)*a(3,2)
         b(2,1) = -(a(2,1)*a(3,3)-a(2,3)*a(3,1))
         b(3,1) =   a(2,1)*a(3,2)-a(2,2)*a(3,1)
         b(1,2) = -(a(1,2)*a(3,3)-a(1,3)*a(3,2))
         b(2,2) =   a(1,1)*a(3,3)-a(1,3)*a(3,1)
         b(3,2) = -(a(1,1)*a(3,2)-a(1,2)*a(3,1))
         b(1,3) =   a(1,2)*a(2,3)-a(1,3)*a(2,2)
         b(2,3) = -(a(1,1)*a(2,3)-a(1,3)*a(2,1))
         b(3,3) =   a(1,1)*a(2,2)-a(1,2)*a(2,1)
         dmat = a(1,1)*b(1,1)+a(1,2)*b(2,1)+a(1,3)*b(3,1) !determinant of A

         if(abs(dmat) > tiny(dmat)) then
            b= b/dmat
         else
            b=0.0_dp
         end if

         Return
      end Function Invert


!***********************************************************************
!  The Subroutines RANnD(RADIUS,X1,Y1,,) generate a pseudo-random
!  spin configuration that is a random vector suitable for the selected
!  spin model:
!      RAN1D(RADIUS,X1)         for    Ising spins
!      RAN2D(RADIUS,X1,Y1)      for    XY spins
!      RAN3D(RADIUS,X1,Y1,Z1)   for    Heisenberg spins
!      RANqD(RADIUS,X1,Y1)      for    q-state Potts planar (XY) spins
!  The Subroutines RAN2D(RADIUS,X1,Y1) and RAN3D(RADIUS,X1,Y1,Z1)
!  originate from the Program Library of CERN Computer Center
!  (CERN - Geneva), with original names RAN2VS and RAN3VS.
!  These Subroutines call no Subroutine.
!  They call the following function :
!  RANF() : this portable real function [Program library of CERN
!           Computer Centre (CERN - Geneva)] is used to generate random
!           numbers between 0 and 1. This function may be slower than
!           dedicated random number generators. Therefore, in order to
!           save CPU time, users should replace RANF() by a call to
!           the equivalent function on the computer they use.
!           For instance, DEC-computer users should call the random
!           generator RAN(I1,I2). For sake of convenience, the calls to
!           this function are included in the current version of the
!           program as comment cards. After implementing and testing
!           the program (see manual), the use of RAN(I1,I2) or of a
!           faster computer random generator, is recommended.

!***********************************************************************
      Subroutine ran1d(radius,x1,y1,z1,n)
      real(kind=sp), intent(in)  :: radius
      real(kind=sp), intent(out) :: x1,y1,z1
      integer,       intent(in)  :: n
!***********************************************************************
!  For a general description of the Subroutines RANnD(RADIUS,X1,Y1...),
!  see the comments just above.
!  RAN1D(RADIUS,X1) generates pseudo-randomly a number +RADIUS or
!  -RADIUS.
!***********************************************************************
         if(ranf()-0.5 > 0) then
           x1 = radius*dx(n)
           y1 = radius*dy(n)
           z1 = radius*dz(n)
         else
           x1 = -radius*dx(n)
           y1 = -radius*dy(n)
           z1 = -radius*dz(n)
         end if
         Return
      end Subroutine ran1d


      Subroutine Ran2d(radius,x1,y1)
      real(kind=sp), intent(in)  :: radius
      real(kind=sp), intent(out) :: x1,y1
!***********************************************************************
!  For a general description of the Subroutines RANnD(RADIUS,X1,Y1...),
!  see the comments just before the Subroutine RAN1D(RADIUS,X1).
!  RAN2D(RADIUS,X1,Y1) generates pseudo-randomly a point on a circle of
!  radius RADIUS.
!  It originates from the Program Library of CERN Computer Center
!  (CERN - Geneva) with the original name RAN2VS.
!***********************************************************************
         real(kind=sp) :: x,y,rr,scal
         do
           x  =  2.0*ranf() - 1.0
           y  =  2.0*ranf() - 1.0
           rr =  x*x + y*y
           if(rr <= 1.0)  exit
         end do
         scal  =  radius / SQRT(rr)
         x1  =  x * scal
         y1  =  y * scal
         Return
      end Subroutine ran2d


      Subroutine ran3d(radius,x1,y1,z1)
      real(kind=sp), intent(in)  :: radius
      real(kind=sp), intent(out) :: x1,y1,z1
      real(kind=sp) :: x,y,z,rr,scal
!***********************************************************************
!  For a general description of the Subroutines RANnD(RADIUS,X1,Y1...),
!  see the comments just before the Subroutine RAN1D(RADIUS,X1).
!  RAN3D(RADIUS,X1,Y1,Z1) generates pseudo-randomly a point at the
!  surface of a sphere of radius RADIUS.
!  It originates from the Program Library of CERN Computer Center
!  (CERN - Geneva) with the original name RAN3VS.
!***********************************************************************
         do
           x  =  2.0*ranf() - 1.0
           y  =  2.0*ranf() - 1.0
           z  =  2.0*ranf() - 1.0
           rr =  x*x + y*y + z*z
           if(rr <= 1.0)  exit
         end do
         scal  =  radius / sqrt(rr)
         x1  =  x * scal
         y1  =  y * scal
         z1  =  z * scal
         Return
      end Subroutine ran3d

      Subroutine ranqd(radius,x1,y1)
      real(kind=sp), intent(in)  :: radius
      real(kind=sp), intent(out) :: x1,y1
      real(kind=sp) :: aq, uran
      integer :: i
!***********************************************************************
!  For a general description of the Subroutines RANnD(RADIUS,X1,Y1...),
!  see the comments just before the Subroutine RAN1D(RADIUS,X1).
!  RANqD(RADIUS,X1,Y1) generates pseudo-randomly a point on a circle of
!  radius RADIUS among q points spaced out of an angle 360/q degrees.
!***********************************************************************
         uran=ranf()
         do i=1,nq
           aq = real(i)/real(nq)
           if(uran < aq) then
             x1 = radius*COS(2*pi*(i-1)/nq)
             y1 = radius*SIN(2*pi*(i-1)/nq)
             exit
           end if
         end do
         Return
      end Subroutine ranqd


      Function ranf() result(r)
        real(kind=sp)   :: r
        call random_number(r)
      end Function ranf

      Subroutine Write_Date_Time(lun,dtim)
        integer,         optional,intent(in) :: lun
        character(len=*),optional,intent(out):: dtim
        !--- Local variables ----!
        character (len=10) :: dat
        character (len=10) :: tim
        call date_and_time(date=dat,time=tim)
        if(present(lun)) &
        write(unit=lun,fmt="(/,4a)") &
          " => Date: ",dat(7:8)//"/"//dat(5:6)//"/"//dat(1:4),      &
            "  Time: ",tim(1:2)//":"//tim(3:4)//":"//tim(5:10)
        if(present(dtim)) &
         dtim="#   Date: "//dat(7:8)//"/"//dat(5:6)//"/"//dat(1:4)//      &
               "  Time: "//tim(1:2)//":"//tim(3:4)//":"//tim(5:10)
        Return
      end Subroutine Write_Date_Time

      Function L_Case(Text) Result (Mtext)
         !---- Argument ----!
         character (len=*), intent(in) :: text
         character (len=len(text))     :: mtext

         !---- Local variables ----!
         integer, parameter :: inc = ICHAR("A") - ICHAR("a")
         integer            :: leng, pos

         mtext=text
         leng=len_trim(mtext)
         do pos=1,leng
            if(mtext(pos:pos) >= "A" .and. mtext(pos:pos) <= "Z")           &
                mtext(pos:pos) = CHAR ( ICHAR(mtext(pos:pos)) - inc )
         end do

         Return
      end Function L_Case

      Function U_Case(Text) Result (Mtext)
         !---- Argument ----!
         character (len=*), intent(in) :: text
         character (len=len(text))     :: mtext

         !---- Local variables ----!
         integer, parameter :: inc = ICHAR("A") - ICHAR("a")
         integer            :: leng, pos

         mtext=text
         leng=len_trim(mtext)
         do pos=1,leng
            if(mtext(pos:pos) >= "a" .and. mtext(pos:pos) <= "z")           &
                mtext(pos:pos) = CHAR ( ICHAR(mtext(pos:pos)) + inc )
         end do

         Return
      end Function U_Case

 End Module mcm_inc

 Module mcmag_input_output_calc
    Use mcm_inc
    implicit none
    public

    contains
      Subroutine write_fst_mcmag(filenam,ier)
        character(len=*), intent(in) :: filenam
        integer,intent(out):: ier
        integer:: n,u,v,w,i_fst=34
        character(len=20) :: atm_text
        real(kind=sp), dimension(3) :: spr,spv,pos
        if(coordinates_read) then
           OPEN(unit=i_fst,FILE=trim(filenam),STATUS='replace',action="write",iostat=ier )
           if(ier /= 0) Return
           write(i_fst,"(a)")"!  File generated by MCMAG"
           write(i_fst,"(a)")"TITLE "//trim(titl1)//"  "//trim(titl2)
           write(i_fst,"(a)")"SPACEG P 1 "
           write(i_fst,"(a,3f12.6,3f12.4)")"CELL ",lu*aax,lv*aay,lw*aaz,aaa,aab,aag
           write(i_fst,"(a)")"  "
           write(i_fst,"(a)") 'BOX   -0.15  1.15   -0.15  1.15    -0.15  1.15'
           write(i_fst,"(a)")"  "
           write(i_fst,"(a)")'{'
           write(i_fst,"(a)")'Lattice P'
           write(i_fst,"(a)")'K    0.00   0.00   0.00'
           write(i_fst,"(a)")'SYMM   x, y, z'
           write(i_fst,"(a)")'MSYM   u, v, w, 0.00'
           DO w=1,lw
             DO v=1,lv
               DO u=1,lu
                 DO n=1,na
                   atm_text=" "
                   spv=[sx(n,u,v,w),sy(n,u,v,w),sz(n,u,v,w)] !Spins of the last configuration in Cartesian Coordinates
                   spr=matmul(ac,spv)                        !Converted to unitary system along a,b,c
                   write(atm_text,"(a,3i2.2)") trim(namt(n))//"_",u-1,v-1,w-1
                   pos=(real([u-1,v-1,w-1])+xyz(:,n))/real([lu,lv,lw])
                   write(i_fst,"(a,3f10.5,a)") "Matom "//trim(atm_text)//"  "//elem(n),pos,'  SCALE 1.0 GROUP'
                   write(i_fst,"(a20,3f10.5,a)") "SkP    1   1 ",spr,'  0.00  0.00  0.00    0.00  '
                 end DO
               end DO
             end DO
           end DO
           write(i_fst,"(a)")'}'
           call flush(i_fst)
           close(unit=i_fst)
        end if
    end Subroutine write_fst_mcmag

    Subroutine Write_Cfl_mcmag(filenam)
        character(len=*), intent(in) :: filenam
        integer:: n,u,v,w
        character(len=20) :: atm_text
        real(kind=sp), dimension(3) :: spr,pos
        if(coordinates_read) then
             OPEN(UNIT=i04,STATUS='replace',FILE=trim(filenam)//'.cfl' )
             write(i04,"(a)")"!  File generated by MCMAG with the last SPIN configuration "
             write(i04,"(a,f8.3)")"!    Temperature: ",t," Kelvin"
             write(i04,"(a,f8.3,a,3f8.3,a)")"!  Applied Field: ",hk," kOe,  along: [",hx0,hy0,hz0,"]"
             write(i04,"(a)")"Title "//trim(titl1)//"  "//trim(titl2)
             write(i04,"(a)")"Spaceg P 1 "
             write(i04,"(a,3f12.6,3f12.4)")"Cell ",lu*aax,lv*aay,lw*aaz,aaa,aab,aag
             write(i04,"(a)")"  "
             write(i04,"(a)") 'Box   -0.15  1.15   -0.15  1.15    -0.15  1.15'
             write(i04,"(a)")"  "
             write(i04,"(a)")'Mag_Structure'
             write(i04,"(a)")'Lattice P'
             write(i04,"(a)")'Kvect    0.00   0.00   0.00'
             write(i04,"(a)")'Symm   x, y, z'
             write(i04,"(a)")'Msym   u, v, w, 0.00'
             DO w=1,lw
               DO v=1,lv
                 DO u=1,lu
                   DO n=1,na
                     atm_text=" "
                     spr=[sx(n,u,v,w),sy(n,u,v,w),sz(n,u,v,w)] !Spins in Crystallographic Coordinates
                     write(atm_text,"(a,3i2.2)") trim(namt(n))//"_",u-1,v-1,w-1
                     pos=(real([u-1,v-1,w-1])+xyz(:,n))/real([lu,lv,lw])
                     write(i04,"(a,3f10.5,a)") "Matom "//trim(atm_text)//"  "//scattf(n),pos,'  SCALE 1.0 GROUP'
                     write(i04,"(a20,3f10.5,a)") "SkP    1   1 ",spr,'  0.00  0.00  0.00    0.00  '
                   end DO
                 end DO
               end DO
             end DO
             write(i04,"(a)")'End_Mag_Structure'
             close(unit=i04)
        end if
    end Subroutine Write_Cfl_mcmag

    Subroutine read_file()
!***********************************************************************
!  This Subroutine reads the input file MYFILE.DAT which contains the
!  topological and magnetic parameters.
!  This Subroutine calls no Subroutine.
!***********************************************************************
        implicit none
        Integer :: nin,nfi,j,i,ier,naco,k
        real(kind=sp) :: smo
        Character (Len=34)  :: Nfila
        Character (Len=132) :: Line
        Character (Len=6)   :: aux_scf

        naco=0
        write(lpt,"(a)")'  ----------------------------------------------------------------------------'
        write(lpt,"(a)")'                               Program McMag'
        write(lpt,"(a)")'           Simulation of Magnetic Structures by a Monte-Carlo Method'
        write(lpt,"(a)")'           McMag: a computer program to simulate magnetic structures'
        write(lpt,"(a)")'          P. Lacorre and J. Pannetier, J. Magn. Magn. Mat. 71 (1987) 63.'
        write(lpt,"(a)")'                     Fortran-90 Version May 2015 (JRC-ILL)'
        write(lpt,"(a)")'        Energy per site Calculated According to the Following Expression :'
        write(lpt,"(a)")"             Ei= - 0.5*Sum(j){Si'[Jij]Sj} + Di(ui'Si)**2) - H'Si "
        write(lpt,"(a)")'  ----------------------------------------------------------------------------'
        call write_Date_Time(lpt)
        write(lpt,"(//a//)") ' Input file : '//trim(nfil)//'.mcm  contains the following data : '
        write(*,*) " => Input File: ",trim(nfil)//".mcm"
        write(lpt,"(1x,a)")titl1
        write(lpt,"(1x,a)")titl2
        write(lpt,"(a)") " "
        nfila = trim(nfil)//'.mcm'
        do
          read(i01,"(a)") line
          line=adjustl(line)
          if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
          read(line,*,iostat=ier) na,jcod,nmot
          if(ier /= 0) then
            write(*,"(a)") " => Error reading the MCM file at the items NA,JCOD,NV !"
            write(*,"(a)") " => Error in Line: "//trim(line)
            stop
          end if
          exit
        end do

        if(((jcod /= 0).and.(jcod /= 1).and.(jcod /= 2))  &
            .or.(na == 0).or.(nmot < 1))   then
            write(*,"(a/a)") " =>  NA and NMOT must be integers and at least equal to 1",&
                             ' =>  JCOD must be equal to 0, 1 or 2'
            stop
        end if

        iani=1

        if(na < 0) then
          iani=-1
          na=-na
        end if


        DO i=1,na
          do
             read(i01,"(a)",iostat=ier) line
             line=adjustl(line)
             if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
             exit
          end do
          if(iani == -1) then
            read(line,*,iostat=ier)nat(i),nvs(nat(i))
            if(ier /= 0) nat(i)=0
          else
            read(line,*,iostat=ier) nat(i),nvs(nat(i)),d(nat(i)),  &
                ddix(nat(i)),ddiy(nat(i)),ddiz(nat(i))
            if(ier /= 0) nat(i)=0
          end if
          if((nat(i) < 1).or.(nvs(nat(i)) < 1).or. (nat(i) > na))  then
               write(*,"(a/a/a)")' => NAT must be an integer between 1 and NA',  &
                                 '    NV must be an integer at least equal to 1 ',&
                                 "    or an error has been produced in reading single ion anisotropy"
               write(*,"(a)") " => Error in Line: "//trim(line)
               stop
          end if
          !reading the name and coordinates of the atoms
          j=index(line,"::")
          if(j /= 0) then
             line=adjustl(line(j+2:))
             j=index(line," ")
             namt(i)=line(1:j)
             elem(i)=line(1:2)
             if(index("0123456789_-()",line(2:2)) /= 0) elem(i)(2:2)=" "
             read(line(j+1:),*,iostat=ier) xyz(:,i)
             if(ier /= 0) then
                coordinates_read=.false.
             else
                coordinates_read=.true.
                write(lpt,"(a,i3,a,3f10.5)") "    Atom #",i,"   "//namt(i)//"  "//elem(i),xyz(:,i)
             end if
          end if

          write(lpt,"(/,1X,'SITE ',i2,' [ D = ',f8.3,' along the direction ',3(f6.3,2X),']',/,9X,'with ',i2,' neighbours :')") &
              nat(i),d(nat(i)), ddix(nat(i)),ddiy(nat(i)),ddiz(nat(i)), nvs(nat(i))

          if(jcod == 0) then          ! JCOD=0 => ISOTROPIC EXCHANGE
            DO j=1,nvs(nat(i))
              do
                 read(i01,"(a)",iostat=ier) line
                 line=adjustl(line)
                 if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
                 exit
              end do
              read(line,*,iostat=ier) nav(nat(i),j),(m(nat(i),j,k),k=1,3), ajjx(nat(i),j)
              if(ier /= 0 .or. (nav(nat(i),j) < 1).or.(nav(nat(i),j) > na) ) then
                 write(*,"(a/a/a)")' => NAV must be an integer between 1 and NA',  &
                                   "    or an error is produced at reading a connetivity line: NAV,Av,Bv,Cv,J"
                 write(*,"(a)") " => Error in Line: "//trim(line)
                 stop
              end if

              write(lpt,"(9X,'Site ',i2,'  in cell (',i2,',',i2,',',i2,')','  with J(',i2,',',i2,') =',f10.3,' K')") &
                    nav(nat(i),j),(m(nat(i),j,k),k=1,3), nat(i),nav(nat(i),j),ajjx(nat(i),j)
              ajjy(nat(i),j)=ajjx(nat(i),j)
              ajjz(nat(i),j)=ajjx(nat(i),j)
            end DO


          else if(jcod == 1) then     ! JCOD=1 => DIAGONAL TENSOR
            DO j=1,nvs(nat(i))
              do
                 read(i01,"(a)",iostat=ier) line
                 line=adjustl(line)
                 if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
                 exit
              end do
              read(line,*,iostat=ier)nav(nat(i),j),(m(nat(i),j,k),k=1,3),  &
                  ajjx(nat(i),j),ajjy(nat(i),j),ajjz(nat(i),j)
              if(ier /= 0 .or. nav(nat(i),j) < 1) then
                 write(*,"(a/a/a)")' => NAV must be an integer between 1 and NA',  &
                                   "    or an error is produced at reading a connectivity line: NAV,Av,Bv,Cv,Jx,Jy,Jz"
                 write(*,"(a)") " => Error in Line: "//trim(line)
                 stop
              end if

              write(lpt,92)nav(nat(i),j),(m(nat(i),j,k),k=1,3),  &
                  nat(i),nav(nat(i),j), ajjx(nat(i),j),ajjy(nat(i),j),ajjz(nat(i),j)
            end DO


          else if(jcod == 2) then     ! JCOD=2 => ANISOTROPIC TENSOR
            DO j=1,nvs(nat(i))
              do
                 read(i01,"(a)",iostat=ier) line
                 line=adjustl(line)
                 if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
                 exit
              end do
              read(line,*,iostat=ier)nav(nat(i),j),(m(nat(i),j,k),k=1,3),  &
                  ajjx(nat(i),j),ajjy(nat(i),j),ajjz(nat(i),j),  &
                  ajxy(nat(i),j),ajxz(nat(i),j),ajyz(nat(i),j),  &
                  ajyx(nat(i),j),ajzx(nat(i),j),ajzy(nat(i),j)
              if(ier/= 0 .or. nav(nat(i),j) < 1) then
                 write(*,"(a/a/a)")' => NAV must be an integer between 1 and NA',  &
                                   "    or an error is produced at reading a connectivity line:", &
                                   "    NAV,Av,Bv,Cv,Jxx,Jyy,Jzz,Jxy,Jxz,Jyz,Jyx,Jzx,Jzy"
                 write(*,"(a)") " => Error in Line: "//trim(line)
                 stop
              end if

              write(lpt,192)nav(nat(i),j),(m(nat(i),j,k),k=1,3),  &
                  nat(i),nav(nat(i),j), ajjx(nat(i),j),ajxy(nat(i),j),ajxz(nat(i),j),  &
                  ajyx(nat(i),j),ajjy(nat(i),j),ajyz(nat(i),j),  &
                  ajzx(nat(i),j),ajzy(nat(i),j),ajjz(nat(i),j)
            end DO

          end if

        end DO

        write(lpt,"(a)")" "
        write(lpt,"(a)")" "
        do
           do
              read(i01,"(a)",iostat=ier) line
              line=adjustl(line)
              if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
              exit
           end do
           read(line,*,iostat=ier) nin,nfi,smo
           i=index(line,"M")
           if(i == 0) then
              i=index(line,"J")
              if(i == 0) then
                aux_scf=" "
              else
                aux_scf=line(i:)
              end if
           else
             aux_scf=line(i:)
           end if
           if(ier/= 0 .or. (nin < 1).or.(nfi < 1).or.(nfi < nin).or. (smo < 0.0)) then
              write(*,"(5(a,/))")" => NI and NF must be integers and at least equal to 1",  &
                                 "    NF must always be larger than or equal to NI", &
                                 "    The largest NF must be equal to NAT", &
                                 "    A spin amplitude must be attributed to any site", &
                                 "    Spin amplitudes must be positive or equal to 0"
              write(*,"(a)") " => Error in Line: "//trim(line)
              stop
           end if
           naco = naco+nfi-nin+1

           DO i=nin,nfi
             amps(i)=smo
             if(len_trim(aux_scf) /= 0 ) then
                scattf(i)= aux_scf
             else
                scattf(i)="M"//u_case(elem(i))//"3"
             end if
             write(lpt,"(22x,a,i2,a,f6.3)")' Spin amplitude ( ',i,' ) = ',amps(i)
          end DO

           if(naco /= na) cycle
           exit
        end do

        DO i=1,na
          d0(i)=SQRT(ddix(i)*ddix(i)+ddiy(i)*ddiy(i)+ddiz(i)*ddiz(i))
          if(d0(i) /= 0) then
            dx(i)=ddix(i)/d0(i)
            dy(i)=ddiy(i)/d0(i)
            dz(i)=ddiz(i)/d0(i)
          end if
        end DO

        !read a,b,c,alpha,beta,gamma
        do
           read(i01,"(a)",iostat=ier) line
           line=adjustl(line)
           if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
           exit
        end do
        read(line,*,iostat=ier) aax,aay,aaz,aaa,aab,aag
        if(ier /= 0) then
            coordinates_read=.false. !fst file cannot be written
            write(*,"(a)") " => Error reading the cell parameters"
            write(*,"(a)") " => Error in Line: "//trim(line)
            stop
        end if
        ! * * * COMPUTATION OF TRANSFORMATION MATRICES AA(I,J), AC(I,J) AND AD(I,J) * * *
        !     Vc= AD Vx  <= crystallographic directions transformed to Cartesian direction
        !     Mx= AC Mc  <= Cartesian moments to crystallographic unitary system (AC=AA^-1)
        call conv_matrix()   !Calculate the matrix AC for converting moment to crystallographic components in any case
        !Printing the matrices passing from orthogonal to crystallographic frames and viceversa
        write(lpt,"(/,a)")  "        Matrices for conversions Cartesian <-> Crystallographic "
        write(lpt,"(/,a)")  "    M(Cart.) = AA . M(Cryst.)               M(Cryst.)= AC .  M(Cart.)"
        do i=1,3
          write(lpt,"(3f10.5,tr10,3f10.5)")  aa(i,:),ac(i,:)
        end do
        write(lpt,"(a)")" "
        92 FORMAT(9X,'Site ',i2,'  in cell (',i2,',',i2,',',i2,')',  &
            '  with coupling (',i2,',',i2,') :',/,15X,  &
            'Jx = ',f8.3,' K , Jy = ',f8.3,' K , Jz = ',f8.3,' K')
        192 FORMAT(9X,'Site ',i2,'  in cell (',i2,',',i2,',',i2,')',/,  &
            15X,'with J tensor (',i2,',',i2,') =   ',3(f8.3,1X),/,  &
            41X,3(f8.3,1X),/,41X,3(f8.3,1X))
        9876 FORMAT(1X,'D(',i2,') = ',f8.3,2X,'( direction ',3(f6.3,2X), ')')
        Return
    end Subroutine read_file


    Subroutine comp_test()
!***********************************************************************
!  This Subroutine tests the validity of connectivity between spins.
!  Every interaction is described twice in the input file (spin i with
!  neighbour j, then spin j with neighbour i).
!  The Subroutine checks that the two descriptions are compatible
!  as well from the topological viewpoint as from the coupling
!  constant viewpoint.
!  Any time an anomaly is detected, an error message is generated,
!  that gives the location of the corresponding error.

!  The test is designed as follows :
!  - TOPOLOGY
!    For any coupling Jij, the consistency of the cell relative
!    indexing (M(i,j,k), k=1,2,3) of neighbouring spins is checked
!    by calculating TCO(k) = M(i,j,k) + M(j,i,k). ifany anomaly is
!    detected (non-zero TCO(k)), an error message is generated, that
!    gives the location of corresponding error (namely M(i,j,k))
!  - INTERACTIONS
!    Following the same scheme, the consistency of interactions Jij
!    is checked by calculating TCOJ* = Jij - Jji for any component
!    of the coupling tensor (* means X,Y,Z,XY,XZ,YX,YZ,ZX or ZY).
!    ifany anomaly is detected (non-zero TCOJ*), an error message
!    is generated, that gives the location of corresponding error
!    (namely J(i,j))
!  Remark : For specific purpose, such as the study of non symmetrical
!  coupling (Jij different from Jji), the last part of the test (at
!  least) should be bypassed. ifyou bypass the test, make sure that
!  your input data are correct !

!  This Subroutine calls no Subroutine.

!***********************************************************************
      integer :: nh, nhp,i,j,k,n,ni,nj,ii,l
      nh=1
      nhp=1

      DO i=1,na

        DO nj=1,nnmax
          pt(nj)=0
        end DO

   do_j:DO  j=1,nvs(nat(i))
          if((jcod == 2).and.(nat(i) == nav(nat(i),j)))GO TO 4446
          DO ni=1,nnmax
            if( nav(nat(i),j) == pt(ni) ) cycle do_j
          end DO
          DO k=1,3
            tco(k)=m(nat(i),j,k)
          end DO
          tcojx=ajjx(nat(i),j)
          if(jcod > 0) then
            tcojy=ajjy(nat(i),j)
            tcojz=ajjz(nat(i),j)
            if(jcod == 2) then
              tcojxy=ajxy(nat(i),j)
              tcojxz=ajxz(nat(i),j)
              tcojyx=ajyx(nat(i),j)
              tcojyz=ajyz(nat(i),j)
              tcojzx=ajzx(nat(i),j)
              tcojzy=ajzy(nat(i),j)
            end if
          end if

          nh=1

          DO n=j+1,nvs(nat(i))
            if( nav(nat(i),n) == nav(nat(i),j) ) then
              DO k=1,3
                tco(k)=tco(k)+m(nat(i),n,k)
              end DO
              nh=-nh
              if(nav(nat(i),n) /= nat(i))nh=1
              tcojx=tcojx+nh*ajjx(nat(i),n)
              if(jcod > 0) then
                tcojy=tcojy+nh*ajjy(nat(i),n)
                tcojz=tcojz+nh*ajjz(nat(i),n)
                if(jcod == 2) then
                  tcojxy=tcojxy+nh*ajxy(nat(i),n)
                  tcojxz=tcojxz+nh*ajxz(nat(i),n)
                  tcojyx=tcojyx+nh*ajyx(nat(i),n)
                  tcojyz=tcojyz+nh*ajyz(nat(i),n)
                  tcojzx=tcojzx+nh*ajzx(nat(i),n)
                  tcojzy=tcojzy+nh*ajzy(nat(i),n)
                end if
              end if
              pt(n)=nav(nat(i),n)
            end if
          end DO
          ii=nav(nat(i),j)
          nhp=1

          DO l=1,nvs(ii)
            if( nav(ii,l) == nat(i) ) then
              DO k=1,3
                tco(k)=tco(k)+m(nav(nat(i),j),l,k)
              end DO
              nhp=-nhp
              if(ii /= nat(i)) nhp=1
              tcojx=tcojx-nhp*ajjx(nav(nat(i),j),l)
              if(jcod > 0) then
                tcojy=tcojy-nhp*ajjy(nav(nat(i),j),l)
                tcojz=tcojz-nhp*ajjz(nav(nat(i),j),l)
                if(jcod > 0) then
                  tcojxy=tcojxy-nhp*ajyx(nav(nat(i),j),l)
                  tcojxz=tcojxz-nhp*ajzx(nav(nat(i),j),l)
                  tcojyx=tcojyx-nhp*ajxy(nav(nat(i),j),l)
                  tcojyz=tcojyz-nhp*ajzy(nav(nat(i),j),l)
                  tcojzx=tcojzx-nhp*ajxz(nav(nat(i),j),l)
                  tcojzy=tcojzy-nhp*ajyz(nav(nat(i),j),l)
                end if
              end if
            end if
          end DO

          DO k=1,3
            if(jcod == 0) then
              if( (tco(k) /= 0).or.(tcojx /= 0) ) then
                write(lpt,131)
                write(itto,131)
                write(lpt,109)nat(i),nav(nat(i),j),k,  &
                    tco(k),nat(i),nav(nat(i),j), tcojx,nat(i),nav(nat(i),j)
                write(itto,109)nat(i),nav(nat(i),j),k,  &
                    tco(k),nat(i),nav(nat(i),j), tcojx,nat(i),nav(nat(i),j)
                STOP
              end if

            else if(jcod == 1) then
              if( (tco(k) /= 0).or.(tcojx /= 0).or.  &
                    (tcojy /= 0).or.(tcojz /= 0) ) then
                write(lpt,131)
                write(itto,131)
                write(lpt,709)nat(i),nav(nat(i),j),k,  &
                    tco(k),nat(i),nav(nat(i),j), tcojx,nat(i),nav(nat(i),j),  &
                    tcojy,nat(i),nav(nat(i),j), tcojz,nat(i),nav(nat(i),j)
                write(itto,709)nat(i),nav(nat(i),j),k,  &
                    tco(k),nat(i),nav(nat(i),j), tcojx,nat(i),nav(nat(i),j),  &
                    tcojy,nat(i),nav(nat(i),j), tcojz,nat(i),nav(nat(i),j)
                STOP
              end if

            else if(jcod == 2) then
              if( (tco(k) /= 0).or.(tcojx /= 0).or.  &
                    (tcojy /= 0).or.(tcojz /= 0).or.  &
                    (tcojxy /= 0).or.(tcojxz /= 0).or.  &
                    (tcojyx /= 0).or.(tcojyz /= 0).or.  &
                    (tcojzx /= 0).or.(tcojzy /= 0) ) then
                write(lpt,131)
                write(itto,131)
                write(lpt,809)nat(i),nav(nat(i),j),k,  &
                    tco(k),nat(i),nav(nat(i),j),  &
                    tcojx,tcojxy,tcojxz,nat(i),nav(nat(i),j),  &
                    tcojyx,tcojy,tcojyz,nat(i),nav(nat(i),j),  &
                    tcojzx,tcojzy,tcojz,nat(i),nav(nat(i),j)
                write(itto,809)nat(i),nav(nat(i),j),k,  &
                    tco(k),nat(i),nav(nat(i),j),  &
                    tcojx,tcojxy,tcojxz,nat(i),nav(nat(i),j),  &
                    tcojyx,tcojy,tcojyz,nat(i),nav(nat(i),j),  &
                    tcojzx,tcojzy,tcojz,nat(i),nav(nat(i),j)
                STOP
              end if
            end if
          end DO

        end DO  do_j

      end DO

      write(lpt,132)
      write(itto,133)
      Return

 4446 write(itto,"(a/a)") ' => Please DO NOT connect sites to themselves with asymmetric coupling',&
                          '    You should double your cell'

      131 FORMAT(/'  ----------------------------------------------------------------------------',/,  &
                  '                         COMPATIBILITY TEST : BAD !!',/,&
                  '                    ****** VERIFY YOUR DATA FILE ! ******',/,  &
                  '  ----------------------------------------------------------------------------')
      132 FORMAT(/'  ----------------------------------------------------------------------------',/,  &
                  '                     COMPATIBILITY TEST : DATA FILE LOOKS OK',/,  &
                  '  ----------------------------------------------------------------------------')
      133 FORMAT( '  ----------------------------------------------------------------------------',/,  &
                  '                     COMPATIBILITY TEST : DATA FILE LOOKS OK',/,  &
                  '  ----------------------------------------------------------------------------')
      109 FORMAT('   Test sym. on  M (',3I2,' ) = ',f5.2,/,'     Test on J (',2I2,' ) = ',f8.3,/,  &
                 ' Dubious connectivity or exchange constant between site ',i2,' and site ',i2,/)
      709 FORMAT('  Test sym. on  M (',3I2,' ) = ',f5.2,/,  &
          '    Test on Jx (',2I2,' ) = ',f8.3,/,  &
          '    Test on Jy (',2I2,' ) = ',f8.3,/,  &
          '    Test on Jz (',2I2,' ) = ',f8.3,/,  &
          ' Dubious connectivity or exchange constant between site ',i2,' and site ',i2,/)
      809 FORMAT('  Test sym. on  M (',3I2,' ) = ',f5.2,/,  &
          '    Test on Jxx,Jxy,Jxz (',2I2,' ) = ',3(f8.3,1X),/,  &
          '    Test on Jyx,Jyy,Jyz (',2I2,' ) = ',3(f8.3,1X),/,  &
          '    Test on Jzx,Jzy,Jzz (',2I2,' ) = ',3(f8.3,1X),/,  &
          ' Dubious connectivity or exchange constant between site ',i2,' and site ',i2,/)
    end Subroutine comp_test


    Subroutine mod_int()

!***********************************************************************

!  This Subroutine allows to modify one or more coupling constant(s)
!  within the sample box, for instance to test the perturbation of
!  the magnetic configuration due to the introduction of impurities
!  in the sample. The Subroutine is activated only ifJCOD=0
!  (isotropic exchange).

!  This Subroutine calls no Subroutine.

!***********************************************************************
      integer :: i,j,k,ja,jb,jba, maj1,maj2,maj3
      im=1
 7706 write(itto,7701)
      read(itti,*)nm(im),ium(im),ivm(im),iwm(im)
      write(iba,*)nm(im),ium(im),ivm(im),iwm(im)

      if(nm(im) /= 0) then
        write(itto,7702)nm(im),nvs(nm(im))
        DO j=1,nvs(nm(im))
          write(itto,7703)j,nav(nm(im),j),(m(nm(im),j,k),k=1,3), ajjx(nm(im),j)
        end DO
        7708   write(itto,7705)
        read(itti,*)numb(im),ajmod(im)
        write(iba,*)numb(im),ajmod(im)
        if(numb(im) == 0)GO TO 7706
        ja=nav(nm(im),numb(im))

        DO jb=1,nvs(ja)
          maj1=m(ja,jb,1)+m(nm(im),numb(im),1)
          maj2=m(ja,jb,2)+m(nm(im),numb(im),2)
          maj3=m(ja,jb,3)+m(nm(im),numb(im),3)
          if( (nav(ja,jb) == nm(im)).and.(maj1 == 0).and.  &
                (maj2 == 0).and.(maj3 == 0) ) then
            jba=jb
          end if
        end DO

        numb(im+1)=jba
        ajmod(im+1)=ajmod(im)
        nm(im+1)=ja
        ium(im+1)=ium(im)+m(nm(im),numb(im),1)
        if(ium(im+1) <= 0) ium(im+1)=ium(im+1)+lu
        if(ium(im+1) > lu) ium(im+1)=ium(im+1)-lu
        ivm(im+1)=ivm(im)+m(nm(im),numb(im),2)
        if(ivm(im+1) <= 0) ivm(im+1)=ivm(im+1)+lv
        if(ivm(im+1) > lv) ivm(im+1)=ivm(im+1)-lv
        iwm(im+1)=iwm(im)+m(nm(im),numb(im),3)
        if(iwm(im+1) <= 0) iwm(im+1)=iwm(im+1)+lw
        if(iwm(im+1) > lw) iwm(im+1)=iwm(im+1)-lw
        im=im+2
        nm(im)=nm(im-2)
        ium(im)=ium(im-2)
        ivm(im)=ivm(im-2)
        iwm(im)=iwm(im-2)
        GO TO 7708

      end if

      im=im-1
      write(itto,7711)
      write(lpt,7711)
      DO i=1,im
        write(itto,7712)nm(i),ium(i),ivm(i),iwm(i),numb(i),ajmod(i)
        write(lpt,7712)nm(i),ium(i),ivm(i),iwm(i),numb(i),ajmod(i)
      end DO
      write(itto,"(/)")
      write(lpt,"(/)")

      7701 FORMAT(1X,' Identify site (N) and cell (U,V,W) [0 0 0 0' ,' to stop] :')
      7702 FORMAT(1X,' Site number',i2,' has ',i2,' neighbours :')
      7703 FORMAT(1X,' Neigh. numb.',i2,' = site',i2,' in relative cell'  &
          , ' (',i2,',',i2,',',i2,') with J =',f8.3,' K')
      7705 FORMAT(1X,' Enter modification (Neigh. numb. and new J) [0 0'  &
          ,' to stop] :')
      7711 FORMAT(/,1X,' The following modifications have been applied'  &
          , ' for the current simulation :',/,' (Original file is preserved)')
      7712 FORMAT(1X,' From site',i2,' in cell (',i2,',',i2,',',i2,')'  &
          ,' to its neigh. numb.',i2,' : new J =',f8.3,' K')

    End Subroutine mod_int


    Subroutine gen_spi()

!***********************************************************************

!  This Subroutine generates or reads the input spin configuration.
!  In the former case, spins are randomly generated.
!  In the latter case, spin values are read from the file MYFILE.INI.
!  The input format of this file is identical to the format of the
!  file MYFILE.SPI which is generated at the end of every simulation and
!  contains the last generated spin configuration of the simulation. Thus
!  a simulation can be split up in several runs.

!  This Subroutine calls the following Subroutines:
!     RANnD(RADIUS,X1,Y1...)

!***********************************************************************
      integer :: u,v,w,ju,jv,jw,n,npassi,npf,npi,jn

      if(iran == 1) then

!*****     FOR ISING SPINS     *****

        DO w=1,lw
          DO v=1,lv
            DO u=1,lu

              if((ians == 'I').or.(ians == 'i')) then
                read(i02,*)
                read(i02,*)ju,jv,jw
                read(i02,*)
              end if

              DO n=1,na
                if((ians == 'R') .or. (ians == 'r')) then
                  call ran1d(amps(n),x,y,z,n)
                  sx(n,u,v,w)=x
                  sy(n,u,v,w)=y
                  sz(n,u,v,w)=z
                else
                  if(iani == -1) then
                    read(i02,*)jn,sx(jn,ju,jv,jw)
                  else
                    read(i02,*)jn,sx(jn,ju,jv,jw),sy(jn,ju,jv,jw), sz(jn,ju,jv,jw)
                  end if
                end if
              end DO

            end DO
          end DO
        end DO


      else if(iran == 2) then

!*****     FOR XY SPINS     *****

        DO w=1,lw
          DO v=1,lv
            DO u=1,lu

              if((ians == 'I').or.(ians == 'i')) then
                read(i02,*)
                read(i02,*)ju,jv,jw
                read(i02,*)
              end if

              DO n=1,na
                if((ians == 'R').or.(ians == 'r')) then
                  call ran2d(amps(n),x,y)
                  sx(n,u,v,w)=x
                  sy(n,u,v,w)=y
                else
                  read(i02,*)jn,sx(jn,ju,jv,jw),sy(jn,ju,jv,jw)
                end if
              end DO

            end DO
          end DO
        end DO


      else if(iran == 3) then

!*****     FOR HEISENBERG SPINS     *****

        DO w=1,lw
          DO v=1,lv
            DO u=1,lu

              if((ians == 'I').or.(ians == 'i')) then
                read(i02,*)
                read(i02,*)ju,jv,jw
                read(i02,*)
                write(itto,*)ju,jv,jw
              end if

              DO n=1,na
                if((ians == 'R').or.(ians == 'r')) then
                  call ran3d(amps(n),x,y,z)
                  sx(n,u,v,w)=x
                  sy(n,u,v,w)=y
                  sz(n,u,v,w)=z
                else
                  read(i02,*)jn,sx(jn,ju,jv,jw),sy(jn,ju,jv,jw), sz(jn,ju,jv,jw)
                end if
              end DO

            end DO
          end DO
        end DO


      else if(iran == 4) then

!*****     FOR q-STATE POTTS PLANAR SPINS     *****

        DO w=1,lw
          DO v=1,lv
            DO u=1,lu

              if((ians == 'I').or.(ians == 'i')) then
                read(i02,*)
                read(i02,*)ju,jv,jw
                read(i02,*)
              end if

              DO n=1,na
                if((ians == 'R').or.(ians == 'r')) then
                  call ranqd(amps(n),x,y)
                  sx(n,u,v,w)=x
                  sy(n,u,v,w)=y
                else
                  read(i02,*)jn,sx(jn,ju,jv,jw),sy(jn,ju,jv,jw)
                end if
              end DO

            end DO
          end DO
        end DO


      end if


!*****     end OF GENERATION AND readING     *****


!*****     OUTPUT     *****

      if((ians == 'R') .or. (ians == 'r')) then
        write(lpt,"(///a//)")  '  Random Initial Spin Configuration '
      else
        write(lpt,"(///a,a//)")'  Initial Spin Configuration read from File: ',trim(nfil)//'.ini'
      end if

      DO w=1,lw
        DO v=1,lv
          DO u=1,lu

            npi=1
            npf=na
            write(lpt ,"(a,i2,a,i2,a,i2,a)")'  CELL (',u,',',v,',',w,')'
            write(lpt,"(a)")

            npassi=0
            do
               if((na-nal*npassi) > nal) npf=npi+nal-1
               npassi=npassi+1
               write(lpt,491)
               DO n=npi,npf
                 write(lpt,496)n
               end DO
               write(lpt,497)(sx(n,u,v,w),n=npi,npf)
               if(iwmx /= 1) then
                 write(lpt,493)(sy(n,u,v,w),n=npi,npf)
                 if(iwmxy /= 1) write(lpt,494)(sz(n,u,v,w),n=npi,npf)
               end if
               write(lpt,595)
               if(npf >= npi+nal-1) then
                 if((npi /= 1).or.(npf /= na)) then
                   npi=npf+1
                   npf=na
                   if(npi <= npf) cycle
                 end if
               end if
               exit
            end do
          end DO
        end DO
      end DO


      496 FORMAT('Site',i2,'   ',$)
      497 FORMAT(/1X,' Sx ',50(1X,f6.3,2X))
      493 FORMAT(1X,' Sy ',50(1X,f6.3,2X))
      494 FORMAT(1X,' Sz ',50(1X,f6.3,2X))
      491 FORMAT('+     ',$)
      595 FORMAT(/)

    end Subroutine gen_spi

    Subroutine mc_cycle()
!***********************************************************************
!  This Subroutine executes the Monte-Carlo loops according to the
!  following scheme:
!
!               |
!               V
!               |
!               |--------->--------.
!               |                  |
!               |---->---.         |
!               |        |         |
!               |   -----------    |
!               |  | spin loop |   |
!               |  | on sample |   |
!               |   -----------    |
!               |        |         |
!               |----<---'    -----------
!               |            | IT loops  |
!               |            | of MCS/S  |
!               |             -----------
!               |                  |
!               |---------<--------'
!               |
!               V
!               |
!  This Subroutine calls the following Subroutines and function:
!  ENn_CALC(N,U,V,W)
!  RANF() : this portable real function [Program library of CERN
!           Computer Centre (CERN - Geneva)] is used to generate random
!           numbers between 0 and 1. This function may be slower than
!           dedicated random number generators. Therefore, in order to
!           save CPU time, users should replace RANF() by a call to
!           the equivalent function on the computer they use.
!           For instance, DEC-computer users should call the random
!           generator RAN(I1,I2). For sake of convenience, the calls to
!           this function are included in the current version of the
!           program as comment cards. After implementing and testing
!           the program (see manual), the use of RAN(I1,I2) or of a
!           faster computer random generator, is recommended.
!***********************************************************************
      real(kind=sp) :: omx,omy,omz,asup,ex,ainf,ax,ay,az
      integer :: ini,itim,n,k,u,v,w
      omx=0.0
      omy=0.0
      omz=0.0
      DO w=1,lw
        DO v=1,lv
          DO u=1,lu
            DO k=1,na
              somx(k,u,v,w)=0.0
              somy(k,u,v,w)=0.0
              somz(k,u,v,w)=0.0
              somx2(k,u,v,w)=0.0
              somy2(k,u,v,w)=0.0
              somz2(k,u,v,w)=0.0
            end DO
          end DO
        end DO
      end DO
      ini=0
      npaj=0

      if(iran == 1) then          ! ISING SPINS

!***** MONTE CARLO LOOPS FOR ISING SPINS *****

        DO  itim=1,it
          DO w=1,lw
            DO v=1,lv
              DO u=1,lu
                DO n=1,na
                  call en1_calc(n,u,v,w)
                  asup=-delta/t-88.029
                  if(asup <= 0.0) then
                    ainf=-delta/t+89.415
                    if(ainf < 0) CYCLE
                    ex=EXP(-delta/t)
                    if(ex-ranf() < 0.0) CYCLE
                  end if
                  en=en+delta
                  sx(n,u,v,w)=x
                  sy(n,u,v,w)=y
                  sz(n,u,v,w)=z
                  if(itim > nom) npaj=npaj+1
                end DO
              end DO
            end DO
          end DO

! * * * MEAN VALUES AND VARIANCE OF MAGNETIC MOMENTS AND ENERGY * * *
! FIRST PART OF CALCULATION: SOMMATION OVER THE SNOM=IT-NOM LAST CYCLES

          if(itim-nom > 0) then
            ini=ini+1
          else
            CYCLE
          end if
          DO w=1,lw
            DO v=1,lv
              DO u=1,lu
                DO k=1,na
                  omx=omx+sx(k,u,v,w)
                  omy=omy+sy(k,u,v,w)
                  omz=omz+sz(k,u,v,w)
                  somx(k,u,v,w)=somx(k,u,v,w)+sx(k,u,v,w)
                  somy(k,u,v,w)=somy(k,u,v,w)+sy(k,u,v,w)
                  somz(k,u,v,w)=somz(k,u,v,w)+sz(k,u,v,w)
                  somx2(k,u,v,w)=somx2(k,u,v,w)+sx(k,u,v,w)**2
                  somy2(k,u,v,w)=somy2(k,u,v,w)+sy(k,u,v,w)**2
                  somz2(k,u,v,w)=somz2(k,u,v,w)+sz(k,u,v,w)**2
                end DO
              end DO
            end DO
          end DO
          enm=enm+en
          en2m=en2m+en*en
          mag2=mag2+omx**2+omy**2+omz**2
          omx=0.0
          omy=0.0
          omz=0.0
        end DO

!***** end OF MONTE CARLO LOOPS FOR ISING SPINS *****

      else if(iran == 2) then     ! XY SPINS

!***** MONTE CARLO LOOPS FOR XY SPINS *****

      DO  itim=1,it
        DO w=1,lw
          DO v=1,lv
            DO u=1,lu
              DO n=1,na
                call en2_calc(n,u,v,w)
                asup=-delta/t-88.029
                if(asup <= 0.0) then
                  ainf=-delta/t+89.415
                  if(ainf < 0) CYCLE
                  ex=EXP(-delta/t)
                  if(ex-ranf() < 0.0) CYCLE
                end if
                en=en+delta
                sx(n,u,v,w)=x
                sy(n,u,v,w)=y
                if(itim > nom) npaj=npaj+1
              end DO
            end DO
          end DO
        end DO

! * * * MEAN VALUES AND VARIANCE OF MAGNETIC MOMENTS AND ENERGY * * *
! FIRST PART OF CALCULATION: SOMMATION OVER THE SNOM=IT-NOM LAST CYCLES

        if(itim-nom > 0) then
          ini=ini+1
        else
          CYCLE
        end if
        DO w=1,lw
          DO v=1,lv
            DO u=1,lu
              DO k=1,na
                omx=omx+sx(k,u,v,w)
                omy=omy+sy(k,u,v,w)
                somx(k,u,v,w)=somx(k,u,v,w)+sx(k,u,v,w)
                somy(k,u,v,w)=somy(k,u,v,w)+sy(k,u,v,w)
                somx2(k,u,v,w)=somx2(k,u,v,w)+sx(k,u,v,w)**2
                somy2(k,u,v,w)=somy2(k,u,v,w)+sy(k,u,v,w)**2
              end DO
            end DO
          end DO
        end DO
        enm=enm+en
        en2m=en2m+en*en
        mag2=mag2+omx**2+omy**2
        omx=0.0
        omy=0.0
        omz=0.0
      end DO

!***** end OF MONTE CARLO LOOPS FOR XY SPINS *****

      else if(iran == 3) then     ! HEISENBERG SPINS

!***** MONTE CARLO LOOPS FOR HEISENBERG SPINS *****

      DO  itim=1,it
        DO w=1,lw
          DO v=1,lv
            DO u=1,lu
              DO n=1,na
                call en3_calc(n,u,v,w)
                asup=-delta/t-88.029
                if(asup <= 0) then
                  ainf=-delta/t+89.415
                  if(ainf < 0)  CYCLE
                  ex=EXP(-delta/t)
                  if(ex-ranf() < 0.0) CYCLE
                endIF
                en=en+delta
                sx(n,u,v,w)=x
                sy(n,u,v,w)=y
                sz(n,u,v,w)=z
                if(itim > nom) npaj=npaj+1
              end DO
            end DO
          end DO
        end DO

! * * * MEAN VALUES AND VARIANCE OF MAGNETIC MOMENTS AND ENERGY * * *
! FIRST PART OF CALCULATION: SOMMATION OVER THE SNOM=IT-NOM LAST CYCLES

        if(itim-nom > 0) then
          ini=ini+1
        else
          CYCLE
        end if
        DO w=1,lw
          DO v=1,lv
            DO u=1,lu
              DO k=1,na
                omx=omx+sx(k,u,v,w)
                omy=omy+sy(k,u,v,w)
                omz=omz+sz(k,u,v,w)
                somx(k,u,v,w)=somx(k,u,v,w)+sx(k,u,v,w)
                somy(k,u,v,w)=somy(k,u,v,w)+sy(k,u,v,w)
                somz(k,u,v,w)=somz(k,u,v,w)+sz(k,u,v,w)
                somx2(k,u,v,w)=somx2(k,u,v,w)+sx(k,u,v,w)**2
                somy2(k,u,v,w)=somy2(k,u,v,w)+sy(k,u,v,w)**2
                somz2(k,u,v,w)=somz2(k,u,v,w)+sz(k,u,v,w)**2
              end DO
            end DO
          end DO
        end DO
        enm=enm+en
        en2m=en2m+en*en
        mag2=mag2+omx**2+omy**2+omz**2
        omx=0.0
        omy=0.0
        omz=0.0
      end DO


!***** end OF MONTE CARLO LOOPS FOR HEISENBERG SPINS *****

      else if(iran == 4) then     ! q-STATE POTTS PLANAR SPINS

!***** MONTE CARLO LOOPS FOR q-STATE PLANAR POTTS SPINS *****

      DO  itim=1,it
        DO w=1,lw
          DO v=1,lv
            DO u=1,lu
              DO n=1,na
                call enq_calc(n,u,v,w)
                asup=-delta/t-88.029
                if(asup <= 0.0) then
                  ainf=-delta/t+89.415
                  if(ainf < 0) CYCLE
                  ex=EXP(-delta/t)
                  if(ex-ranf() < 0.0) CYCLE
                end if
                en=en+delta
                sx(n,u,v,w)=x
                sy(n,u,v,w)=y
                if(itim > nom) npaj=npaj+1
              end DO
            end DO
          end DO
        end DO

! * * * MEAN VALUES AND VARIANCE OF MAGNETIC MOMENTS AND ENERGY * * *
! FIRST PART OF CALCULATION: SOMMATION OVER THE SNOM=IT-NOM LAST CYCLES

        if(itim-nom > 0) then
          ini=ini+1
        else
          CYCLE
        end if
        DO w=1,lw
          DO v=1,lv
            DO u=1,lu
              DO k=1,na
                omx=omx+sx(k,u,v,w)
                omy=omy+sy(k,u,v,w)
                somx(k,u,v,w)=somx(k,u,v,w)+sx(k,u,v,w)
                somy(k,u,v,w)=somy(k,u,v,w)+sy(k,u,v,w)
                somx2(k,u,v,w)=somx2(k,u,v,w)+sx(k,u,v,w)**2
                somy2(k,u,v,w)=somy2(k,u,v,w)+sy(k,u,v,w)**2
              end DO
            end DO
          end DO
        end DO
        enm=enm+en
        en2m=en2m+en*en
        mag2=mag2+omx**2+omy**2
        omx=0.0
        omy=0.0
        omz=0.0
      end DO


!***** end OF MONTE CARLO LOOPS FOR q-STATE POTTS SPINS *****


      end if

      DO w=1,lw
        DO v=1,lv
          DO u=1,lu
            DO k=1,na
              somx(k,u,v,w)=somx(k,u,v,w)/snom
              comx(k,u,v,w)=somx(k,u,v,w)
              somy(k,u,v,w)=somy(k,u,v,w)/snom
              somz(k,u,v,w)=somz(k,u,v,w)/snom
              somx2(k,u,v,w)=somx2(k,u,v,w)/snom
              somy2(k,u,v,w)=somy2(k,u,v,w)/snom
              somz2(k,u,v,w)=somz2(k,u,v,w)/snom
              ax=somx2(k,u,v,w)-somx(k,u,v,w)**2
              if(ax < 0) ax=0
              ay=somy2(k,u,v,w)-somy(k,u,v,w)**2
              if(ay < 0) ay=0
              az=somz2(k,u,v,w)-somz(k,u,v,w)**2
              if(az < 0) az=0
              somx2(k,u,v,w)=SQRT(ax)
              somy2(k,u,v,w)=SQRT(ay)
              somz2(k,u,v,w)=SQRT(az)
            end DO
          end DO
        end DO
      end DO

    end Subroutine mc_cycle


    Subroutine en_tot(L,en1)
!***********************************************************************
!  This Subroutine computes the total energy of the system.
!  This Subroutine calls the following Subroutine:
!     EN3_CALC(N,U,V,W)
!***********************************************************************
      integer, intent(in) :: L
      real(kind=dp),intent(out) :: en1
      real(kind=dp) :: en0
      integer :: i,n,u,v,w,ier
      real(kind=sp) :: aux
      character(len=80) :: time_line,filenam
      nmult=2
      DO i=1,na
        amps2(i)=0
      end DO
      !For L=3 the copy the average magnetic moments into spin variables
      !has been done before calling the Subroutine
      x=0.0; y=0.0; z=0.0 !Initialize the spin components
      en0=0               !Needed for calculating the total energy with the
      nfip=0              !current spin configuration [Sx,Sy,Sz](n,u,v,w)
      DO w=1,lw
        DO v=1,lv
          DO u=1,lu
            DO n=1,na
              call en3_calc(n,u,v,w)
              en0=en0-delta
            end DO
          end DO
        end DO
      end DO
      en=en0/2
      en1=en/nfa
      if(l == 1) then
        write(lpt ,"(a,f12.5)")'       *  Energy of the starting spin configuration = ',en1
        write(itto,"(a,f12.5)")'       *  Energy of the starting spin configuration = ',en1

      else if(l == 2) then
        write(lpt ,"(a,f12.5)")'       *  Energy of the last spin configuration = ',en1
        write(itto,"(a,f12.5)")'       *  Energy of the last spin configuration = ',en1

        do i=1,10
         call write_fst_mcmag('simann.fst',ier)
         if(ier == 0) exit
        end do
        write(filenam,"(a,i3.3)") trim(nfil),job
        call Write_Cfl_mcmag(filenam)
        iwrt=iwrt-1
        call cpu_time(cpu_par)
        cpu= cpu_par-cpu
        remaining=iwrt*cpu/60.0/60.0  !remaining time in decimal hours
        time_line(1:80)=" "
        write(unit=time_line(1:10),fmt="(i3,a)") int(remaining)," hours,"
        remaining=(remaining-int(remaining))*60.0 !remaining time in decimal minutes
        write(unit=time_line(12:21),fmt="(i2,a)") int(remaining)," minutes"
        aux=(remaining - int(remaining))*60.0 !Number of seconds remaining
        write(unit=time_line(23:50),fmt="(f8.3,a)") aux," seconds"
        write(unit=*,fmt="(a,f8.2,a,a,/)")" => Partial time:",cpu*60.0, &
               " seconds  ->  Approx. remaining time: ",trim(time_line)
        cpu=cpu_par
      else if(l == 3) then
        write(lpt ,"(a,f12.5)")'       *  Energy of the average spin configuration = ',en1
        write(itto,"(a,f12.5)")' => Energy of the average spin configuration = ',en1
      end if
      nfip=1
      DO i=1,na
        amps2(i)=amps(i)
      end DO
      nmult=1
    end Subroutine en_tot


    Subroutine const_funct()

!***********************************************************************
!  This Subroutine calculates the constraint function Fc of the system
!  (for isotropic exchange only), according to the formula:
!  (with the convention that a vector V' represents the transposed
!  form of a vector V)
!
!       Fc = -E/Eb  with  E  = - JijSi'Sj             (energy)
!                    and  Eb = -|Jij||Si||Sj|      (basic energy)
!
!  Further details about the constraint function are given in the
!  following paper:
!  The constaint functions: an attempt to evaluate the constraint
!  rate inside compounds that undergo ordered magnetic frustration.
!  P. Lacorre, J. Phys. C: Solid State Phys. 20 (1987) L775-L781
!
!  This Subroutine calls the following Subroutine:
!     BOUND_COND
!
!***********************************************************************

      real(kind=sp) :: fcnu,fcde
      integer :: k,n,u,v,w
      fcnu = 0
      fcde = 0
      DO w=1,lw
        DO v=1,lv
          DO u=1,lu
            DO n=1,na
              fjsx(n)=0
              fjsy(n)=0
              fjsz(n)=0
              fjsm(n)=0
              DO k=1,nvs(n)

                call bound_cond(n,k,u,v,w)

                fjsx(n)=fjsx(n)+ajxx(n,k)*comx(nav(n,k),nu(k),nv(k),nw(k))
                fjsy(n)=fjsy(n)+ajyy(n,k)*somy(nav(n,k),nu(k),nv(k),nw(k))
                fjsz(n)=fjsz(n)+ajzz(n,k)*somz(nav(n,k),nu(k),nv(k),nw(k))
                fjsm(n)=fjsm(n)+ABS(ajxx(n,k)) *tmom(nav(n,k),nu(k),nv(k),nw(k))
              end DO

! * * *   COMPUTATION OF THE NUMERATOR (FCNU=2*(MEAN ENERGY))   * * *
! * * *   AND DENOMINATOR (FCDE=-2*(BASIC MEAN ENERGY)) OF FC   * * *

             fcnu=fcnu-comx(n,u,v,w)*fjsx(n)-somy(n,u,v,w)*fjsy(n)-somz(n,u,v,w)*fjsz(n)
             fcde=fcde+tmom(n,u,v,w)*fjsm(n)
            end DO
          end DO
        end DO
      end DO
      if(fcde < 0.000001) fcde=0.000001
      fconst=fcnu/fcde
      rc=50*(1+fconst)

    end Subroutine const_funct


    Subroutine pro_rota()
!***********************************************************************
!  This Subroutine computes, at the end of simulation, the projection
!  of magnetic moments onto the crystal axes, and rotates the whole
!  system in order to align a given magnetic moment along the desired
!  direction.
!  Calculation for rotation are performed within the orthonormal coord-
!  inate system, then magnetic moments are projected onto the crystal-
!  lographic reference frame.
!  The output file MYFILE.LIS stores :
!  - the components of magnetic moments inside the crystallographic
!    reference frame before rotation
!  - the components of magnetic moments inside the crystallographic
!    reference frame after rotation
!
!  The following parameters are used in the Subroutine :
!
!                    TRANSFORMATION MATRICES
!         between orthonormal coordinate system (o.c.s.)
!         and crystallographic reference frame (c.r.f.)
!  ( ref. : V.A.LIOPO_Sov. Phys. Crystallogr. 30 (1985) 687)
!
!  AA : transformation matrix (c.r.f.) ---> (o.c.s.) for magnetic moments
!  AC=INV(AA) : transformation matrix (o.c.s.) ---> (c.r.f.)
!  AD : transformation matrix (c.r.f.) ---> (o.c.s.) for direction
!
!                           VECTORS
!  SORX,SORY,SORZ : components of the magnetic moment taken as reference
!  DORX,DORY,DORZ : new direction of the magnetic moment
!  UORX,UORY,UORZ : rotation axis ( UOR = SOR^DOR )
!  OM = angle(SOR,DOR)  [ SIOM = SIN(OM) , COOM = COS(OM) ]
!
!                       ROTATION FORMULA
!                       ----------------
!  ( ref.: J.BASS_Cours de Mathematiques T.I f.1 MASSON(1977) )
!                       ( p 130 - ex 36 )
!
!  ifS = arbitrary vector
!     S'= transformation of S by the rotation
!     U = normalised UOR
!
!         S' = (1-COOM)*(U.S)*U + COOM*S + SIOM*(U^S)
!         -------------------------------------------
!
!
!  This Subroutine calls the following Subroutine:
!     conv_matrix
!
!***********************************************************************
      integer       :: ier,k,n,npi,npf,npassa,npassb,u,v,w
      real(kind=sp) :: sorx,sory,sorz,dor,sorm, sor,coom, uorx, &
                       uory,uorz, uor, siom, pscal
      real(kind=dp),dimension(3) :: vecto,vectx

      !read(i01,*)aax,aay,aaz,aaa,aab,aag  !Removed from here for reading cell event ifPRO_ROTA is not invoked


! * * * NOR = INDEX OF MOMENT TAKEN AS REFERENCE (CELL (1,1,1)) * * *
      if(.not. batch) then
         DO
           write(itto,"(a)") ' => Do you want to redefine the orientation of magnetic moments'
           write(itto,"(a)") '    in the crystal cell ?'
           write(itto,"(a)") '              0    : do not'
           write(itto,"(a)") '           N =< NA : ordinal number of the magnetic moment to be'
           write(itto,"(a)") '                     used to calculate the new orientation'
           write(itto,"(a)",advance='no') ' => Option: '
           read(itti,*,iostat=ier) nor
           if(ier /= 0) cycle
           if((nor < 0) .or. (nor > na)) CYCLE
           EXIT
         end DO
         write(iba,*)nor
      end if
      !--------------------
      if(nor /= 0) then
      !--------------------
        sorx=comx(nor,1,1,1)
        sory=somy(nor,1,1,1)
        sorz=somz(nor,1,1,1)
        if(.not. batch) then
           Do
             write(itto,"(a,i2,a)",advance='no')' => Give new orientation of magnetic moment of SITE ',nor, &
                              ' in the crystal cell: '
             read(itti,*,iostat=ier) dorx,dory,dorz
             if(ier /= 0) cycle
             Exit
           end Do
           write(iba,*)dorx,dory,dorz
        end if
         ! DORX,DORY,DORZ : NEW DIRECTION OF MOMENT NOR INSIDE THE CRYSTAL. REFERENCE

        vectx=[dorx,dory,dorz]  !Crystallographic direction
        dorix=dorx
        doriy=dory
        doriz=dorz
        vecto=matmul(ad,vectx)  !In Cartesian frame  Vc= AD Vx

        dorx=vecto(1) !dorx*ad(1,1)+dory*ad(1,2)+dorz*ad(1,3)   This is an error
        dory=vecto(2) !dorx*ad(2,1)+dory*ad(2,2)+dorz*ad(2,3)
        dorz=vecto(2) !dorx*ad(3,1)+dory*ad(3,2)+dorz*ad(3,3)

        ! DORX,DORY,DORZ : NEW DIRECTION OF MOMENT NOR INSIDE THE ORTHO. SYSTEM

        ! * * * CALCULATION INSIDE THE ORTHONORMAL COORDINATE SYSTEM * * *

        dor=SQRT(dorx*dorx+dory*dory+dorz*dorz)
        sorm=SQRT(sorx*sorx+sory*sory+sorz*sorz)
        sor=dorx*sorx+dory*sory+dorz*sorz
        coom=sor/(dor*sorm)
        uorx=sory*dorz-sorz*dory
        uory=sorz*dorx-sorx*dorz
        uorz=sorx*dory-sory*dorx
        uor=SQRT(uorx*uorx+uory*uory+uorz*uorz)
        siom=uor/(dor*sorm)
        uorx=uorx/uor
        uory=uory/uor
        uorz=uorz/uor

        DO w=1,lw
          DO v=1,lv
            DO u=1,lu
              DO k=1,na
                pscal= uorx*comx(k,u,v,w) +uory*somy(k,u,v,w)+uorz*somz(k,u,v,w)
                somrx(k,u,v,w)= (1-coom)*pscal*uorx + coom*comx(k,u,v,w)  &
                                + siom*( uory*somz(k,u,v,w) -uorz*somy(k,u,v,w) )
                somry(k,u,v,w)= (1-coom)*pscal*uory + coom*somy(k,u,v,w)  &
                                + siom*( uorz*comx(k,u,v,w) -uorx*somz(k,u,v,w) )
                somrz(k,u,v,w)= (1-coom)*pscal*uorz + coom*somz(k,u,v,w)  &
                                + siom*( uorx*somy(k,u,v,w) -uory*comx(k,u,v,w) )

               !   *   SOMRX           COMPONENTS OF MAGNETIC MOMENTS   *
               !   *   SOMRY   ------> INSIDE THE ORTHONORMAL SYSTEM    *
               !   *   SOMRZ           AFTER ROTATION                   *


                somrx2(k,u,v,w)= (1-coom)*pscal*uorx + coom*somx2(k,u,v,w)  &
                                 + siom*( uory*somz2(k,u,v,w) -uorz*somy2(k,u,v,w) )
                somry2(k,u,v,w)= (1-coom)*pscal*uory + coom*somy2(k,u,v,w)  &
                                 + siom*( uorz*somx2(k,u,v,w) -uorx*somz2(k,u,v,w) )
                somrz2(k,u,v,w)= (1-coom)*pscal*uorz + coom*somz2(k,u,v,w)  &
                                 + siom*( uorx*somy2(k,u,v,w) -uory*somx2(k,u,v,w) )

                !   *   SOMRX2          VARIANCE OF THE COMPONENTS OF       *
                !   *   SOMRY2  ------> MAGNETIC MOMENTS INSIDE THE         *
                !   *   SOMRZ2          ORTHONORMAL SYSTEM AFTER ROTATION   *


              end DO
            end DO
          end DO
        end DO


! * * * end OF CALCULATION INSIDE THE ORTHONORMAL SYSTEM * * *

! * * * COMPUTATION INSIDE THE CRISTALLOGRAPHIC REFERENCE FRAME * * *

!   *   PROJECTION OF MAGNETIC MOMENTS AFTER ROTATION   *
!                  (SOMRX,SOMRY,SOMRZ)

        DO w=1,lw
          DO v=1,lv
            DO u=1,lu
              DO k=1,na
                 vecto=[somrx(k,u,v,w),somry(k,u,v,w),somrz(k,u,v,w)]  !Mx= AC Mc
                 vectx=matmul(ac,vecto)  !Below there was an error
                 somrx(k,u,v,w)= vectx(1)!ac(1,1)* somrx(k,u,v,w) +ac(1,2)* somry(k,u,v,w)+ac(1,3)* somrz(k,u,v,w)
                 somry(k,u,v,w)= vectx(2)!ac(2,1)* somrx(k,u,v,w) +ac(2,2)* somry(k,u,v,w)+ac(2,3)* somrz(k,u,v,w)
                 somrz(k,u,v,w)= vectx(3)!ac(3,1)* somrx(k,u,v,w) +ac(3,2)* somry(k,u,v,w)+ac(3,3)* somrz(k,u,v,w)
                 vecto=[somrx2(k,u,v,w),somry2(k,u,v,w),somrz2(k,u,v,w)]
                 vectx=matmul(ac,vecto)
                                         !Below there was an error
                somrx2(k,u,v,w)= vectx(1)!ac(1,1)*somrx2(k,u,v,w) +ac(1,2)*somry2(k,u,v,w)+ac(1,3)*somrz2(k,u,v,w)
                somry2(k,u,v,w)= vectx(2)!ac(2,1)*somrx2(k,u,v,w) +ac(2,2)*somry2(k,u,v,w)+ac(2,3)*somrz2(k,u,v,w)
                somrz2(k,u,v,w)= vectx(3)!ac(3,1)*somrx2(k,u,v,w) +ac(3,2)*somry2(k,u,v,w)+ac(3,3)*somrz2(k,u,v,w)
              end DO
            end DO
          end DO
        end DO
      !--------------------
      end if !nor /= 0
      !--------------------

!   *   PROJECTION OF MAGNETIC MOMENTS BEFORE ROTATION   *
!                      (COMX,SOMY,SOMZ)

      DO w=1,lw
        DO v=1,lv
          DO u=1,lu
            DO k=1,na
               vecto=[comx(k,u,v,w),somy(k,u,v,w),somz(k,u,v,w)] !Cartesian
               vectx=matmul(ac,vecto) ! Xtallographic: Below there was an error
               comx(k,u,v,w)= vectx(1)!ac(1,1)* comx(k,u,v,w) +ac(1,2)* somy(k,u,v,w)+ac(1,3)* somz(k,u,v,w)
               somy(k,u,v,w)= vectx(2)!ac(2,1)* comx(k,u,v,w) +ac(2,2)* somy(k,u,v,w)+ac(2,3)* somz(k,u,v,w)
               somz(k,u,v,w)= vectx(3)!ac(3,1)* comx(k,u,v,w) +ac(3,2)* somy(k,u,v,w)+ac(3,3)* somz(k,u,v,w)

               vecto=[somx2(k,u,v,w),somy2(k,u,v,w),somz2(k,u,v,w)]
               vectx=matmul(ac,vecto)
                                      !Below there was an error
              somx2(k,u,v,w)= vectx(1)!ac(1,1)*somx2(k,u,v,w) +ac(1,2)*somy2(k,u,v,w)+ac(1,3)*somz2(k,u,v,w)
              somy2(k,u,v,w)= vectx(2)!ac(2,1)*somx2(k,u,v,w) +ac(2,2)*somy2(k,u,v,w)+ac(2,3)*somz2(k,u,v,w)
              somz2(k,u,v,w)= vectx(3)!ac(3,1)*somx2(k,u,v,w) +ac(3,2)*somy2(k,u,v,w)+ac(3,3)*somz2(k,u,v,w)
            end DO
          end DO
        end DO
      end DO

! ***** end OF CALCULATION FOR PROJECTION AND ROTATION *****

! ***** OUTPUT RESULTS OF PROJECTION AND ROTATION *****

      write(lpt,517)aax,aay,aaz,aaa,aab,aag
      write(lpt,"(/)")
      write(lpt,618)
      DO w=1,lw
        DO v=1,lv
          DO u=1,lu
            DO n=1,na
              isigx(n)=NINT(1000*somx2(n,u,v,w))
              isigy(n)=NINT(1000*somy2(n,u,v,w))
              isigz(n)=NINT(1000*somz2(n,u,v,w))
            end DO
            npi=1
            npf=na
            write(lpt,498)u,v,w
            write(lpt,"(a)")

            npassa=0
 3412       if((na-nal*npassa) > nal) npf=npi+nal-1
            npassa=npassa+1
            write(lpt,491)
            DO n=npi,npf
              write(lpt,496)n
            end DO
            !write(itto,"(a)")
            write(lpt,"(a)")
            write(lpt,497)(comx(n,u,v,w),n=npi,npf)
            write(lpt,487)(isigx(n),n=npi,npf)
            write(lpt,493)(somy(n,u,v,w),n=npi,npf)
            write(lpt,487)(isigy(n),n=npi,npf)
            write(lpt,494)(somz(n,u,v,w),n=npi,npf)
            write(lpt,487)(isigz(n),n=npi,npf)
            write(lpt,"(/)")
            if(npf >= npi+nal-1) then
              if( (npi /= 1).or.(npf /= na) ) then
                npi=npf+1
                npf=na
                if(npi < npf) GO TO 3412
              end if
            end if
          end DO
        end DO
      end DO

      if(nor == 0) Return
      write(lpt,"(/)")
      write(lpt,638)nor,dorix,doriy,doriz
      write(lpt,"(/)")
      write(lpt,628)
      DO w=1,lw
        DO v=1,lv
          DO u=1,lu
            npi=1
            npf=na
            write(lpt,498)u,v,w
            write(lpt,"(a)")

            npassb=0
 2412       if((na-nal*npassb) > nal) npf=npi+nal-1
            npassb=npassb+1
            write(lpt,491)
            DO n=npi,npf
              write(lpt,496)n
            end DO
            write(lpt,"(a)")
            write(lpt,497)(somrx(n,u,v,w),n=npi,npf)
            write(lpt,493)(somry(n,u,v,w),n=npi,npf)
            write(lpt,494)(somrz(n,u,v,w),n=npi,npf)
            write(lpt,"(/)")
            if(npf >= npi+nal-1) then
              if( (npi /= 1).or.(npf /= na) ) then
                npi=npf+1
                npf=na
                if(npi > npf) GO TO 2412
              end if
            end if
          end DO
        end DO
      end DO


      613 FORMAT(1X,3(f8.4,1X),6X,3(f8.4,1X))
      496 FORMAT('Site',i2,'   ',$)
      497 FORMAT(1X,' Mx ',50(1X,f6.3,2X))
      493 FORMAT(1X,' My ',50(1X,f6.3,2X))
      494 FORMAT(1X,' Mz ',50(1X,f6.3,2X))
      487 FORMAT(5X,50(3X,i4,2X))
      491 FORMAT('+     ',$)
      498 FORMAT(1X,' CELL (',i2,',',i2,',',i2,')')
        517 FORMAT(1X,' Crystal CELL parameters :',/,5X,' a = ',f9.4,  &
            /,5X,' b = ',f9.4,/,5X,' c = ',f9.4,/,5X,' alpha = ',f9.4,  &
            /,5X,' beta  = ',f9.4,/,5X,' gamma = ',f9.4)
        618 FORMAT(1X,' COMPONENTS OF THE MAGNETIC MOMENTS IN THE CRYSTAL'  &
            , ' CELL ',/,'   *************************************'  &
            , '***************',/)
        638 FORMAT(1X,' ***** MOMENTS ARE ROTATED TO ALIGN MOMENT OF SITE ',  &
            i2,' ALONG THE DIRECTION *****',/,13X,'***** ',  &
            3(f7.3,1X),'OF THE CRYSTAL CELL  *****')
        628 FORMAT(1X,' COMPONENTS OF THE MAGNETIC MOMENTS IN THE CRYSTAL'  &
            , ' CELL AFTER ROTATION',/,'   ***********************'  &
            , '********************************************',/)
    end Subroutine pro_rota

    Subroutine out_res()
!***********************************************************************
!  This Subroutine outputs the averaged characteristics in files
!  MYFILE.LIS and MYFILE.RES (if activated) at the end of every
!  temperature/field loop.
!
!  The calculated averaged characteristics are the following :
!  - mean total energy (ENMA)
!  - mean total magnetization (AMMCA)
!  - mean total magnetization along the applied field (AMPARA)
!  - mean magnetization on three selected sites of cell (1,1,1)
!  - specific heat (CVA)
!  - magnetic susceptibility (SUSA)
!  - inverse susceptibility (SUAM1)
!
!  The specific heat and magnetic susceptibility are computed according
!  to the formula :
!  CVA = (<E**2>-<E>**2)/T**2    (where E represents the energy)
!  SUSA = (<M**2>-<M>**2)/T      (where M represents the magnetization)
!
!  This Subroutine calls no Subroutine.
!***********************************************************************

      real(kind=sp) :: paj
      real(kind=sp) :: xm,ym,zm,susa,suam1,cva, &
                       ammc,ampara,ammca
      real(kind=dp) :: enma,enm2

      paj=100.0*real(npaj)/(snom*real(nta))

      enm=enm/snom
      enm2=enm*enm
      en2m=en2m/snom
      mag2=mag2/snom
      ammc=SQRT(amx*amx+amy*amy+amz*amz)

      enma=enm/nfa
      ampara=hdix*amx+hdiy*amy+hdiz*amz
      ammca=ammc/nfa
      ampara=ampara/nfa
      if(ammc /= 0.0) then
        xm=amx/ammc
        ym=amy/ammc
        zm=amz/ammc
      else
        xm=0.0
        ym=0.0
        zm=0.0
      end if
      susa=(mag2-ammc**2)/t
      susa=susa/nfa
      if(susa > 0.0) then
        suam1=1.0/susa
        if(ABS(suam1) > 9999.999) suam1=9999.999
      end if
      cva=(en2m-enm2)/(t*t)
      cva=cva/nfa
      if(cva > 9999.999) cva=9999.999
      enm=0
      en2m=0
      mag2=0
      ammc=0
      if(last == 1) last=0
      if(jcod /= 0) then
        write(lpt,993)paj,xm,ym,zm,hx0,hy0,hz0
        write(itto,993)paj,xm,ym,zm,hx0,hy0,hz0
      else
        write(lpt,693)paj,fconst,rc,xm,ym,zm,hx0,hy0,hz0
        write(itto,693)paj,fconst,rc,xm,ym,zm,hx0,hy0,hz0
      693 FORMAT(7X,'*  Rate of accepted jumps : ',f7.3,' %',/,  &
          7X,'*  Fc = ',f8.5,'   -   Rc = ',f7.3,' %',/,  &
          7X,'*  MTOT   along the direction ',3(1X,f6.3),/,  &
          7X,'*  M//    along the direction ',3(1X,f6.3))
      end if
      write(itto,263)j1,j2,j3
      write(lpt,263)j1,j2,j3
      write(itto,264)susa,suam1,cva,enma,ammca,  &
          tmom(j1,1,1,1),tmom(j2,1,1,1),tmom(j3,1,1,1), ampara
      write(lpt,264)susa,suam1,cva,enma,ammca,  &
          tmom(j1,1,1,1),tmom(j2,1,1,1),tmom(j3,1,1,1), ampara
      if((ansrf == 'Y').or.(ansrf == 'y')) then
        write(i03,265)t,hk,suam1,cva,enma,ammca,  &
            tmom(j1,1,1,1),tmom(j2,1,1,1),tmom(j3,1,1,1), ampara,fconst
      end if


      993 FORMAT(7X,'*  Rate of accepted jumps : ',f7.3,' %',/,  &
          7X,'*  MTOT   along the direction ',3(1X,f6.3),/,  &
          7X,'*  M//    along the direction ',3(1X,f6.3))
      264 FORMAT(7X,'*',f9.4,1X,f8.3,1X,f8.3,1X,f10.2,1X,f7.3,3(1X,f6.3), 1X,f7.3)
      265 FORMAT(3X,f7.2,1X,f7.2,1X,f8.3,1X,f8.3,1X,f10.2,1X,f7.3,  &
          3(1X,f6.3),1X,f7.3,1X,f6.3)
      263 FORMAT(7X,'*    SUSC',4X,'1/SUS',4X,'SP. HT.',6X,'EN',5X,'MTOT',  &
          1X,3(1X,'M(',i2,')'),4X,'M//')

    end Subroutine out_res


    Subroutine out_conf()
!***********************************************************************
!  This Subroutine output the configuration of magnetic moments at
!  current temperature and field.
!  This Subroutine calls no Subroutine.
!***********************************************************************
      integer :: npi,npf,k,n,l,u,v,w
      logical :: prt,prta,prte

      amx=0.0
      amy=0.0
      amz=0.0
      prt=((ansnev == 'E').or.(ansnev == 'e'))
      prta=((ansnev == 'A').or.(ansnev == 'a'))
      prte= (prt .or. prta) .and. (last == 1)

      if( prte) then

        write(lpt,"(/a/a/)") &
          '                    Mean Configuration of Magnetic Moments',   &
          '                            ---------------------'
      end if

      DO w=1,lw
        DO v=1,lv
          DO u=1,lu

            DO k=1,na
              tmom(k,u,v,w)=SQRT( somx(k,u,v,w)**2 +somy(k,u,v,w)**2  &
                  +somz(k,u,v,w)**2 )
            end DO
            npi=1
            npf=na

            ! Modification of the original ifblock!  (caused by illegal goto 1412!)

            if( prta .or. prt)  then

                if( prte) then !Print only at the end
                   write( lpt,"(a,i2,a,i2,a,i2,a,/)")'  CELL (', u, ',', v, ',', w, ')'
                   write( lpt,"(3x,50(a,i2))")("   Site",n,n=npi,npf)
                end if

              DO k=npi,npf
                isigx(k)=NINT(1000*somx2(k,u,v,w))
                isigy(k)=NINT(1000*somy2(k,u,v,w))
                isigz(k)=NINT(1000*somz2(k,u,v,w))
              end DO

              if(prte) then !Print only at the end
                 write(lpt,"(a,50(1X,f6.3,2X))") "   Mx",(somx(n,u,v,w),n=npi,npf)
                 write(lpt,"(5X,50(3X,i4,2X))")          (isigx(n),n=npi,npf)
                 if(iwmx /= 1) then
                    write(lpt,"(a,50(1X,f6.3,2X))") "   My",(somy(n,u,v,w),n=npi,npf)
                    write(lpt,"(5X,50(3X,i4,2X))")          (isigy(n),n=npi,npf)
                    if(iwmxy /= 1) then
                       write(lpt,"(a,50(1X,f6.3,2X))") "   Mz",(somz(n,u,v,w),n=npi,npf)
                       write(lpt,"(5X,50(3X,i4,2X))")          (isigz(n),n=npi,npf)
                    end if
                 end if
              end if
              do k=npi,npf
                if(tmom(k,u,v,w) > 0.00001) then
                  somx(k,u,v,w)=( ABS(somx(k,u,v,w))*somx2(k,u,v,w)  &
                      +ABS(somy(k,u,v,w))*somy2(k,u,v,w)  &
                      +ABS(somz(k,u,v,w))*somz2(k,u,v,w) ) /tmom(k,u,v,w)
                else
                  somx(k,u,v,w)=amps(k)
                end if
                isigm(k)=NINT(1000*somx(k,u,v,w))
              end do
              if(prte) then !Print only at the end
                if(iwmx /= 1) then
                   write(lpt,"(a,50(1X,f6.3,2X))") " Mtot",(tmom(n,u,v,w),n=npi,npf)
                   write(lpt,"(5X,50(3X,i4,2X))")          (isigm(n),n=npi,npf)
                end if
                write(lpt,"(a)")
              end if

              DO l=npi,npf
                amx=amx+comx(l,u,v,w)
                amy=amy+somy(l,u,v,w)
                amz=amz+somz(l,u,v,w)
              end DO

            end if

          end DO
        end DO
      end DO

    end Subroutine out_conf


    Subroutine bound_cond(n,k,u,v,w)
      integer, intent(in) :: n,k,u,v,w

!***********************************************************************
!  This Subroutine implements the boundary conditions for spins at
!  the edges of the sample. Three kinds of boundary conditions are
!  available:
!
!  - free edges:
!    ----------
!    spins at any edge of the sample are considered as free of nei-
!    ghbours outside of the sample. The sample is then representa-
!    tive of a real small crystallite delimited by surfaces
!
!  - periodic boundary conditions (p.b.c.):
!    ----------------------------
!    spins at one edge of the sample are connected to spins at the
!    opposite edge of the sample. This condition imposes a 3D perio-
!    dicity to the system. then the system may be thought of as an
!    infinite size crystal with an elementary cell equal to the
!    sample dimension.
!
!  - mixed boundary conditions:
!    -------------------------
!    this new kind of boundary conditions has been introduced to
!    simulate helimagnets with a propagation axis of the helix.
!    It corresponds to free edges along one direction (axis W) and
!    p.b.c along the other two directions.
!
!     This Subroutine calls no Subroutine.
!***********************************************************************
      nu(k)=u+m(n,k,1)
      nv(k)=v+m(n,k,2)
      nw(k)=w+m(n,k,3)
      if(ianb < 0) then
        GO TO    97
      else if(ianb == 0) then
        GO TO   199
      else
        GO TO    99
      end if

!   * MIXED BOUNDARY CONDITIONS   *

      97   if(nu(k) > 0) then
        GO TO   910
      end if
      900     nu(k)=nu(k)+lu
      GO TO 930
      910     if(lu-nu(k) < 0) then
        GO TO   920
      else
        GO TO   930
      end if
      920       nu(k)=nu(k)-lu
      930       if(nv(k) > 0) then
        GO TO   950
      end if
      940         nv(k)=nv(k)+lv
      GO TO 970
      950         if(lv-nv(k) < 0) then
        GO TO   960
      else
        GO TO   970
      end if
      960           nv(k)=nv(k)-lv
      970           if(nw(k) > 0) then
        GO TO   990
      end if
      980             nw(k)=nwmax+1
      GO TO 911
      990             if(lw-nw(k) < 0) then
        GO TO   901
      else
        GO TO   911
      end if
      901               nw(k)=nwmax+1
      911               GO TO 98

!   * PERIODIC BOUNDARY CONDITIONS   *

      99 if(nu(k) > 0) then
        GO TO   110
      end if
      100     nu(k)=nu(k)+lu
      GO TO 130
      110     if(lu-nu(k) < 0) then
        GO TO   120
      else
        GO TO   130
      end if
      120       nu(k)=nu(k)-lu
      130       if(nv(k) > 0) then
        GO TO   150
      end if
      140         nv(k)=nv(k)+lv
      GO TO 170
      150         if(lv-nv(k) < 0) then
        GO TO   160
      else
        GO TO   170
      end if
      160           nv(k)=nv(k)-lv
      170           if(nw(k) > 0) then
        GO TO   190
      end if
      180             nw(k)=nw(k)+lw
      GO TO 210
      190             if(lw-nw(k) < 0) then
        GO TO   200
      else
        GO TO   210
      end if
      200               nw(k)=nw(k)-lw
      210               GO TO 98

!   * FREE EDGES   *

      199   if(nu(k) > 0) then
        GO TO   310
      end if
      300     nu(k)=numax+1
      GO TO 330
      310     if(lu-nu(k) < 0) then
        GO TO   320
      else
        GO TO   330
      end if
      320       nu(k)=numax+1
      330       if(nv(k) > 0) then
        GO TO   350
      end if
      340         nv(k)=nvmax+1
      GO TO 370
      350         if(lv-nv(k) < 0) then
        GO TO   360
      else
        GO TO   370
      end if
      360           nv(k)=nvmax+1
      370           if(nw(k) > 0) then
        GO TO   390
      end if
      380            nw(k)=nwmax+1
      GO TO 98
      390             if(lw-nw(k) < 0) then
        GO TO   400
      else
        GO TO    98
      end if
      400               nw(k)=nwmax+1
      98               CONTINUE

    end Subroutine bound_cond

!***********************************************************************
!       General description of Subroutines ENn_CALC(N,U,V,W)
!
!  The Subroutines ENn_CALC(N,U,V,W) calculate the energy difference
!  between the new randomly generated configuration (NEW) and the old
!  configuration (OLD), expressed as DELTA = E(OLD) - E(NEW)
!  according to the energy formula :
!  (with the convention that a vector V' represents the transposed
!  form of a vector V)
!
!  E = - 0.5*Sum(j){Si'[Jij]Sj} + Di(ui'Si)**2) - H'Si
!                                              AJXX AJXY AJXZ
!  [Jij] is the exchange tensor with elements  AJYX AJYY AJYZ
!                                              AJZX AJZY AJZZ
!
!  The exchange term Si'[Jij]Sj can be written Si'Jsi where
!  Jsi = [Jij]Sj is the vector (AJSX,AJSY,AJSZ).
!  The calculation of DELTA is made in two steps:
!      - first is calculated the term Jsi, which corresponds to the
!        microscopic molecular field at site i, and that is obviously
!        the same for the two configurations of spin i
!      _ then DELTA difference is calculated according to the above
!        formula.
!
!  The four Subroutines ENn_CALC(N,U,V,W) concern different spin
!  models, namely:
!      the Ising model                       with  EN1_CALC(N,U,V,W)
!      the XY model                          with  EN2_CALC(N,U,V,W)
!      the Heisenberg model                  with  EN3_CALC(N,U,V,W)
!      the q-state Potts planar (XY) model   with  ENq_CALC(N,U,V,W)
!  Only one of them is activated at every run of the program.
!
!  These Subroutines call the following Subroutines:
!     BOUND_COND
!     RANnD(RADIUS,X1,Y1,,)
!***********************************************************************


    Subroutine en1_calc(n,u,v,w)
      integer, intent(in) :: n,u,v,w

!***********************************************************************
!  See the general description of Subroutines ENn_CALC(N,U,V,W)
!  just above.
!  EN1_CALC(N,U,V,W) deals with Ising spins.
!***********************************************************************
      integer :: i,k

      ajsx(n)=0
      ajsy(n)=0
      ajsz(n)=0
      DO k=1,nvs(n)
        ajxx(n,k)=ajjx(n,k)
        ajyy(n,k)=ajjy(n,k)
        ajzz(n,k)=ajjz(n,k)

        if((ansmod == 'Y').or.(ansmod == 'y')) then


!*****    LOCAL MODIFICATION OF COUPLING (IF ANY)    ******


          DO i=1,im
            if( (w == iwm(i)).and.(v == ivm(i)).and.(u == ium(i))  &
                  .and.(n == nm(i)).and.(k == numb(i)) ) then
              ajxx(n,k)=ajmod(i)
              ajyy(n,k)=ajmod(i)
              ajzz(n,k)=ajmod(i)
            end if
          end DO


!*****    end OF MODIFICATION    *****


        end if

        call bound_cond(n,k,u,v,w)


!*****    COMPUTE DELTA    *****

        if( (nav(n,k) /= n).or.(nu(k) /= u).or.(nv(k) /= v)  &
              .or.(nw(k) /= w) ) then
          ajsx(n)=ajsx(n)+ajxx(n,k)*sx(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajxy(n,k)*sy(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajxz(n,k)*sz(nav(n,k),nu(k),nv(k),nw(k))
          ajsy(n)=ajsy(n)+ajyy(n,k)*sy(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajyx(n,k)*sx(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajyz(n,k)*sz(nav(n,k),nu(k),nv(k),nw(k))
          ajsz(n)=ajsz(n)+ajzz(n,k)*sz(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajzx(n,k)*sx(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajzy(n,k)*sy(nav(n,k),nu(k),nv(k),nw(k))
        end if
      end DO

      if(nfip /= 0) then
        call ran1d (amps2(n),x,y,z,n)
      end if

      delta = (sx(n,u,v,w)-x)*(ajsx(n)+(nmult*hx))  &
          +(sy(n,u,v,w)-y)*(ajsy(n)+(nmult*hy)) +(sz(n,u,v,w)-z)*(ajsz(n)+(nmult*hz))  &
          -nmult*d(n) * ( ( dx(n)*sx(n,u,v,w)  &
          +dy(n)*sy(n,u,v,w) +dz(n)*sz(n,u,v,w) )**2  &
          -( dx(n)*x + dy(n)*y + dz(n)*z )**2 )

    end Subroutine en1_calc



    Subroutine en2_calc(n,u,v,w)
      integer, intent(in) :: n,u,v,w
!***********************************************************************
!  See the general description of Subroutines EN*_CALC(N,U,V,W)
!  just before the Subroutine EN1_CALC(N,U,V,W).
!  EN2_CALC(N,U,V,W) concerns XY spins.
!***********************************************************************
      integer :: k,i
      ajsx(n)=0.0
      ajsy(n)=0.0
      ajsz(n)=0.0

      DO k=1,nvs(n)
        ajxx(n,k)=ajjx(n,k)
        ajyy(n,k)=ajjy(n,k)
        ajzz(n,k)=ajjz(n,k)


!*****    LOCAL MODIFICATION OF COUPLING (IF ANY)    ******


        if((ansmod == 'Y').or.(ansmod == 'y')) then
          DO i=1,im
            if( (w == iwm(i)).and.(v == ivm(i)).and.(u == ium(i))  &
                  .and.(n == nm(i)).and.(k == numb(i)) ) then
              ajxx(n,k)=ajmod(i)
              ajyy(n,k)=ajmod(i)
              ajzz(n,k)=ajmod(i)
            end if
          end DO


!*****    end OF MODIFICATION    *****


        end if

        call bound_cond(n,k,u,v,w)


!*****    COMPUTE DELTA    *****

        if( (nav(n,k) /= n).or.(nu(k) /= u).or.(nv(k) /= v)  &
              .or.(nw(k) /= w) ) then
          ajsx(n)=ajsx(n)+ajxx(n,k)*sx(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajxy(n,k)*sy(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajxz(n,k)*sz(nav(n,k),nu(k),nv(k),nw(k))
          ajsy(n)=ajsy(n)+ajyy(n,k)*sy(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajyx(n,k)*sx(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajyz(n,k)*sz(nav(n,k),nu(k),nv(k),nw(k))
          ajsz(n)=ajsz(n)+ajzz(n,k)*sz(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajzx(n,k)*sx(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajzy(n,k)*sy(nav(n,k),nu(k),nv(k),nw(k))
        end if
      end DO

      if(nfip /= 0) then
        call ran2d(amps2(n),x,y)
      end if

      delta = (sx(n,u,v,w)-x)*(ajsx(n)+(nmult*hx))  &
          +(sy(n,u,v,w)-y)*(ajsy(n)+(nmult*hy)) +(sz(n,u,v,w)-z)*(ajsz(n)+(nmult*hz))  &
          -nmult*d(n) * ( ( dx(n)*sx(n,u,v,w)  &
          +dy(n)*sy(n,u,v,w) +dz(n)*sz(n,u,v,w) )**2  &
          -( dx(n)*x + dy(n)*y + dz(n)*z )**2 )

    end Subroutine en2_calc



    Subroutine en3_calc(n,u,v,w)
      integer, intent(in) :: n,u,v,w
!***********************************************************************
!  See the general description of Subroutines EN*_CALC(N,U,V,W)
!  just before the Subroutine EN1_CALC(N,U,V,W).
!  EN3_CALC(N,U,V,W) concerns Heisenberg spins.
!***********************************************************************
      integer :: k,i
      ajsx(n)=0
      ajsy(n)=0
      ajsz(n)=0

      DO k=1,nvs(n)
        ajxx(n,k)=ajjx(n,k)
        ajyy(n,k)=ajjy(n,k)
        ajzz(n,k)=ajjz(n,k)


!*****    LOCAL MODIFICATION OF COUPLING (IF ANY)    ******


        if((ansmod == 'Y').or.(ansmod == 'y')) then
          DO i=1,im
            if( (w == iwm(i)).and.(v == ivm(i)).and.(u == ium(i))  &
                  .and.(n == nm(i)).and.(k == numb(i)) ) then
              ajxx(n,k)=ajmod(i)
              ajyy(n,k)=ajmod(i)
              ajzz(n,k)=ajmod(i)
            end if
          end DO


!*****    end OF MODIFICATION    *****


        end if

        call bound_cond(n,k,u,v,w)


!*****    COMPUTE DELTA    *****

        if( (nav(n,k) /= n).or.(nu(k) /= u).or.(nv(k) /= v)  &
              .or.(nw(k) /= w) ) then
          ajsx(n)=ajsx(n)+ajxx(n,k)*sx(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajxy(n,k)*sy(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajxz(n,k)*sz(nav(n,k),nu(k),nv(k),nw(k))
          ajsy(n)=ajsy(n)+ajyy(n,k)*sy(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajyx(n,k)*sx(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajyz(n,k)*sz(nav(n,k),nu(k),nv(k),nw(k))
          ajsz(n)=ajsz(n)+ajzz(n,k)*sz(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajzx(n,k)*sx(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajzy(n,k)*sy(nav(n,k),nu(k),nv(k),nw(k))
        end if
      end DO

      if(nfip /= 0) then
        call ran3d (amps2(n),x,y,z)
      end if

      delta = (sx(n,u,v,w)-x)*(ajsx(n)+(nmult*hx))  &
          +(sy(n,u,v,w)-y)*(ajsy(n)+(nmult*hy)) +(sz(n,u,v,w)-z)*(ajsz(n)+(nmult*hz))  &
          -nmult*d(n) * ( ( dx(n)*sx(n,u,v,w)  &
          +dy(n)*sy(n,u,v,w) +dz(n)*sz(n,u,v,w) )**2  &
          -( dx(n)*x + dy(n)*y + dz(n)*z )**2 )

    end Subroutine en3_calc


    Subroutine enq_calc(n,u,v,w)
!***********************************************************************
!  See the general description of Subroutines EN*_CALC(N,U,V,W)
!  just before the Subroutine EN1_CALC(N,U,V,W).
!  ENq_CALC(N,U,V,W) concerns q-state Potts planar (XY) spins.
!***********************************************************************
      integer, intent(in) :: n,u,v,w
      integer :: k,i

      ajsx(n)=0
      ajsy(n)=0
      ajsz(n)=0

      DO k=1,nvs(n)
        ajxx(n,k)=ajjx(n,k)
        ajyy(n,k)=ajjy(n,k)
        ajzz(n,k)=ajjz(n,k)


        !*****    LOCAL MODIFICATION OF COUPLING (IF ANY)    ******


        if((ansmod == 'Y').or.(ansmod == 'y')) then
          DO i=1,im
            if( (w == iwm(i)).and.(v == ivm(i)).and.(u == ium(i))  &
                  .and.(n == nm(i)).and.(k == numb(i)) ) then
              ajxx(n,k)=ajmod(i)
              ajyy(n,k)=ajmod(i)
              ajzz(n,k)=ajmod(i)
            end if
          end DO


         !*****    end OF MODIFICATION    *****


        end if

        call bound_cond(n,k,u,v,w)


        !*****    COMPUTE DELTA    *****

        if( (nav(n,k) /= n).or.(nu(k) /= u).or.(nv(k) /= v)  &
              .or.(nw(k) /= w) ) then
          ajsx(n)=ajsx(n)+ajxx(n,k)*sx(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajxy(n,k)*sy(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajxz(n,k)*sz(nav(n,k),nu(k),nv(k),nw(k))
          ajsy(n)=ajsy(n)+ajyy(n,k)*sy(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajyx(n,k)*sx(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajyz(n,k)*sz(nav(n,k),nu(k),nv(k),nw(k))
          ajsz(n)=ajsz(n)+ajzz(n,k)*sz(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajzx(n,k)*sx(nav(n,k),nu(k),nv(k),nw(k))  &
              +ajzy(n,k)*sy(nav(n,k),nu(k),nv(k),nw(k))
        end if

      end DO

      if(nfip /= 0) then
        call ranqd (amps2(n),x,y)
      end if

      delta = (sx(n,u,v,w)-x)*(ajsx(n)+(nmult*hx))  &
          +(sy(n,u,v,w)-y)*(ajsy(n)+(nmult*hy)) +(sz(n,u,v,w)-z)*(ajsz(n)+(nmult*hz))  &
          -nmult*d(n) * ( ( dx(n)*sx(n,u,v,w)  &
          +dy(n)*sy(n,u,v,w) +dz(n)*sz(n,u,v,w) )**2  &
          -( dx(n)*x + dy(n)*y + dz(n)*z )**2 )
    end Subroutine enq_calc

 End Module mcmag_input_output_calc



!***********************************************************************
!                  *******************************
!                  *                             *
!                  *        MAIN PROGRAM         *
!                  *                             *
!                  *******************************
 Program MCMAG

      Use mcm_inc
      Use mcmag_input_output_calc
      implicit none
      integer :: nres,icou,i,j,n,ou3,ier
      real(kind=sp) :: hm0,hi,hki,tci,ta,t_ini,t_fin,t_glob,t_part
      real(kind=dp) :: energy,e_aver,temp
      real(kind=sp),dimension(3) :: pos, spv,spr
      integer :: u,v,w,narg
      character(len=20)  :: atm_text=" ",keyword
      character(len=120) :: line=" "

      narg=COMMAND_ARGUMENT_COUNT()
      if(narg > 0) then
           nfil=" "
           call GET_COMMAND_ARGUMENT(1,nfil)
           i=index(nfil,".")
           if(i /= 0) nfil=nfil(1:i-1)
           batch=.true.
      end if

      nres=0
      write(itto,"(a)")'  ----------------------------------------------------------------------------'
      write(itto,"(a)")'                               Program McMag'
      write(itto,"(a)")'           Simulation of Magnetic Structures by a Monte-Carlo Method'
      write(itto,"(a)")'           McMag: a computer program to simulate magnetic structures'
      write(itto,"(a)")'          P. Lacorre and J. Pannetier, J. Magn. Magn. Mat. 71 (1987) 63.'
      write(itto,"(a)")'                     Fortran-90 Version May 2015 (JRC-ILL)'
      write(itto,"(a)")'        Energy per site Calculated According to the Following Expression :'
      write(itto,"(a)")"             Ei= - 0.5*Sum(j){Si'[Jij]Sj} + Di(ui'Si)**2) - H'Si "
      write(itto,"(a)")'  ----------------------------------------------------------------------------'
      write(itto,"(a)")' '
      write(itto,"(a)")' '
      if(.not. batch) then
         write(itto,"(a)",advance="no")' => Input file name ? : '
         read(itti,"(a)") nfil
      end if
      Open(Unit=i01,File=trim(nfil)//'.mcm',Status='OLD',iostat=ier)
      if(ier /= 0) then
        write(*,"(a)") " => Error opening the file: "//trim(nfil)//'.mcm'
        stop
      end if
      read(i01,"(a)")titl1    ! read First Title of the Input File
      read(i01,"(a)")titl2    ! read Second Title of the Input File
      Open(Unit=lpt,STATUS='unknown',File=trim(nfil)//'.lis')

      call read_file()   ! read the Data File and
                         ! Parameters of the Simulation

      call comp_test()   ! Test of Internal Compatibility
                         ! Concerning Topology and Interactions


      if(batch) then
         do
           read(i01,"(a)",iostat=ier) line
           if(ier == 0) then
             write(lpt,"(a)") "    mcm-line => "//trim(line)
             line=adjustl(l_case(line))
             if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
             exit
           else
             write(*,"(a)") " => Premature end of file (SpinModel): "//trim(nfil)//".mcm"
             stop
           end if
         end do       !123456789
         j=index(line,"spinmodel")
         if(j /= 0) then
           keyword=adjustl(line(j+9:))
           j=index(keyword," ")
           Select Case(keyword(1:j-1))
            Case("ising")
              iran=1
            Case("xy")
              iran=2
            Case("heisenberg")
              iran=3
            Case("q-potts")
              iran=4
              read(keyword(j:),*) nq
            Case default
              write(*,"(a)") " => The keyword SpinModel should followed by: Ising, XY, Heisenberg or q-Potts !"
              stop
           end Select
         else
           write(*,"(a)") " => The keyword: SpinModel, followed by Ising, XY, Heisen, or q-Potts, is needed!"
           stop
         end if
      else
         write(itto,"(a)",advance='no') ' => Ising(1), XY(2) Heisenberg(3) or q-State Potts planar(4) spins?: '
                                        ! ISING, XY, HEISENBERG OR q-STATE
                                        ! POTTS PLANAR SPINS ?
         read(itti,708)iran
      end if
      if((iran < 1).or.(iran > 4)) then
        write(*,"(a)") " => Model ",iran," not supported!"
        stop
      end if

      if(iran == 4 .and. .not. batch) then            ! q-STATE POTTS PLANAR SPINS
        do
          write(itto,"(a)",advance='no')  ' => q-Value (Integer)?: '
          read(itti,*) nq
          if(nq < 2) cycle
          exit
        end do
      end if

      Select Case(iran)
        Case(1)            ! Ising Spins
           spimod='ISING'
           icou = 0
           DO i=1,na
             if(d0(i) == 0) then
               dx(i)=1.
               icou = icou+1
             end if
           end DO
           if(icou == na) iwmx=1

        Case(2)      ! XY Spins
           spimod='XY PLANAR'
           iwmxy=1

        Case(3)           ! Heisenberg Spins
        spimod='HEISENBERG'

        Case(4)       ! q-State Potts Planar Spins
           spimod='-STATE POTTS'
           iwmxy=1
      end Select

      if(.not. batch) then
         Open(Unit=Iba,Status='unknown',FILE=trim(nfil)//'.rec')
         write(Iba,4003) Nfil,iran
         if(Iran == 4) then
           write(Iba,*)Nq
         end if
      end if

      if(batch) then
         do
           read(i01,"(a)",iostat=ier) line
           if(ier == 0) then
             write(lpt,"(a)") "    mcm-line => "//trim(line)
             line=adjustl(line)
             if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
             exit
           else
             write(*,"(a)") " => Premature end of file (Title): "//trim(nfil)//".mcm"
             stop
           end if
         end do       !123456789
         j=index(line," ")
         keyword=l_case(line(1:j-1))
         titl="Simulation using MCMAG"
         if(trim(keyword) == "title") titl=line(j+1:)
      else
         write(itto,"(a)",advance='no')  ' => Title of simulation ?: '
         read(itti,"(a)") titl
         write(iba,"(a)") trim(titl)
      end if

      titl3 = spimod//' Spins - '//trim(titl)
      if(batch) then
         do
           read(i01,"(a)",iostat=ier) line
           if(ier == 0) then
             write(lpt,"(a)") "    mcm-line => "//trim(line)
             line=adjustl(line)
             if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
             exit
           else
             write(*,"(a)") " => Premature end of file (Ncells): "//trim(nfil)//".mcm"
             stop
           end if
         end do       !123456789
         j=index(line," ")
         keyword=l_case(line(1:j-1))
         if(trim(keyword) == "ncells") then
            read(line(j+1:),*,iostat=ier) lu,lv,lw
            if(ier /= 0) then
              write(*,"(a)") " => The keyword Ncells should be followed by the number of cells along a, b and c!"
              stop
             end if
         else
            write(*,"(a)") " => The keyword Ncells is neede here !"
            stop
         end if
      else
         do
           write(itto,"(a)")           ' => Size of the sample box along the three axes? '
           write(itto,"(a,3i4,a)",advance='no') '        ( Maximal values: ',numax,nvmax,nwmax,'):  '  ! SIZE OF THE SAMPLE ?
           read(itti,*,iostat=ier)lu,lv,lw
           if((lu > numax).or.(lv > nvmax).or.(lw > nwmax) .or. ier/=0) cycle
           if((lu < 1).or.(lv < 1).or.(lw < 1)) then
             write(itto,888)
             cycle
           end if
           exit
         end do
         write(iba,*)lu,lv,lw
      end if
      nma=lu*lv*lw
      nta=na*nma
      ansmod='N'
      if(jcod == 0 .and. .not. batch) then
        do
           write(itto,"(a)",advance='no')' => Modify some interactions in the sample box (Y or N)?: '
           read(itti,"(a)")ansmod
           if((ansmod /= 'Y').and.(ansmod /= 'y').and.  &
               (ansmod /= 'N').and.(ansmod /= 'n')) cycle
           exit
        end do
        write(iba,"(a)")ansmod
        if((ansmod == 'Y').or.(ansmod == 'y')) then

          call mod_int()    ! Possibility to modify locally
                            ! some interactions (for isotropic exchange only)
        end if
      end if

      if(batch) then
         do
           read(i01,"(a)",iostat=ier) line
           if(ier == 0) then
             write(lpt,"(a)") "    mcm-line => "//trim(line)
             line=adjustl(line)
             if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
             exit
           else
             write(*,"(a)") " => Premature end of file (InitConf): "//trim(nfil)//".mcm"
             stop
           end if
         end do       !123456789
         j=index(line," ")
         keyword=l_case(line(1:j-1))
         if(trim(keyword) == "initconf") then
           ians=adjustl(line(j+1:))
         else
            write(*,"(a)") " => The keyword InitConf (followed by R or I) is neede here !"
            stop
         end if
      else
         do
           write(itto,"(a)",advance='no') ' => Initial spin configuration :'            ! INITIAL SPIN CONFIGURATION ?
           write(itto,"(a,a,a)",advance="no") ' => Random(R) or from the input file ',trim(nfil)//'.ini',' (I)?:  '
           read(itti,"(a)")ians
           if((ians /= 'R').and.(ians /= 'r').and.  &
               (ians /= 'I').and.(ians /= 'i')) cycle
           exit
         end do
         write(iba,"(a)") ians
      end if

      if((ians == 'I') .or. (ians == 'i')) then
        OPEN(UNIT=i02,STATUS='OLD',FILE=trim(nfil)//'.ini',iostat=ier)
        if(ier /= 0) then
          write(*,"(a)") " => Error opening the file "//trim(nfil)//'.ini'
          stop
        end if
        read(i02,2222)itit1,itit2,itit3
      end if

      call gen_spi()  ! read or Generate the Initial
                      ! Configuration of Spins

      if(batch) then
         do
           read(i01,"(a)",iostat=ier) line
           if(ier == 0) then
             write(lpt,"(a)") "    mcm-line => "//trim(line)
             line=adjustl(line)
             if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
             exit
           else
             write(*,"(a)") " => Premature end of file (Boundary): "//trim(nfil)//".mcm"
             stop
           end if
         end do       !123456789
         j=index(line," ")
         keyword=l_case(line(1:j-1))
         if(trim(keyword) == "boundary") then
           bcond=adjustl(l_case(line(j+1:)))
           if(index(bcond,"free") /= 0) ianb=0
           if(index(bcond,"peri") /= 0) ianb=1
           if(index(bcond,"mixe") /= 0) ianb=-1
         else
            write(*,"(a)") " => The keyword Boundary (followed by Free, Periodic or Mixed) is neede here !"
            stop
         end if
      else
         write(itto,"(a)",advance='no') ' => Free edges(0), Periodic(1) or mixed boundary conditions(-1)?: '
         read(itti,889,iostat=ier) ianb
         write(iba,889)ianb
      end if

      if(ianb < 0) then            ! MIXED BOUNDARY CONDITIONS
        bcond = 'MIXED'

      else if(ianb == 0) then       ! FREE EDGES
        bcond = 'FREE EDGES'

      else if(ianb > 0) then       ! PERIODIC BOUNDARY CONDITIONS
        bcond = 'PERIODIC'

      end if

      if(batch) then
         do
           read(i01,"(a)",iostat=ier) line
           if(ier == 0) then
             write(lpt,"(a)") "    mcm-line => "//trim(line)
             line=adjustl(line)
             if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
             exit
           else
             write(*,"(a)") " => Premature end of file (Scale): "//trim(nfil)//".mcm"
             stop
           end if
         end do       !123456789
         j=index(line," ")
         keyword=l_case(line(1:j-1))
         if(trim(keyword) == "scale") then
           avchar=adjustl(l_case(line(j+1:)))
           infa=2
           if(index(avchar,"samp") /= 0) infa=1
           if(index(avchar,"cell") /= 0) infa=2
           if(index(avchar,"site") /= 0) infa=3
           if(index(avchar,"mole") /= 0) infa=4
         else
            write(*,"(a)") " => The keyword Scale (followed by Samp, Cell, Site or Mole) is neede here !"
            stop
         end if
      else
        do
          write(itto,"(a)") ' => Scaling of averaged characteristics of sample ? :'
          write(itto,"(a)") '          1 = per sample'
          write(itto,"(a)") '          2 = per basic unit cell (input file)'
          write(itto,"(a)") '          3 = per site'
          write(itto,"(a)") '          4 = per mole'
          write(itto,"(a)",advance='no') ' => Option: '
          read(itti,708,iostat=ier) infa
          if(ier /= 0) cycle
          if((infa < 1).or.(infa > 4)) cycle
          exit
        end do
        write(iba,708) infa
      end if


      if(infa == 1) then            ! 1 = PER SAMPLE
        nfa=1
        avchar = 'SAMPLE'
      else if(infa == 2) then       ! 2 = PER UNIT CELL
        nfa=nma
        avchar = 'UNIT CELL'
      else if(infa == 3) then       ! 3 = PER SITE
        nfa=nta
        avchar = 'SITE'
      else if(infa == 4) then       ! 4 = PER MOLE
        nfa=nma*nmot
        avchar = 'MOLE'
      end if

      call en_tot(1, energy)                 ! CALCULATE THE TOTAL ENERGY
! WITH THE CURRENT MAGNETIC FIELD


!***** SUMMARIZE SIMULATION PARAMETERS *****

      write(lpt,596)
      write(lpt,2003)titl
      write(lpt,2015)lu,lv,lw,na,nmot,avchar,trim(bcond)
      if(nq /= 0) then
        write(lpt,2014)nq,spimod
      else
        write(lpt,2016) spimod
      end if

!*****


      if(batch) then
         do
           read(i01,"(a)",iostat=ier) line
           if(ier == 0) then
             write(lpt,"(a)") "    mcm-line => "//trim(line)
             line=adjustl(line)
             if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
             exit
           else
             write(*,"(a)") " => Premature end of file (Sites): "//trim(nfil)//".mcm"
             stop
           end if
         end do       !123456789
         j=index(line," ")
         keyword=l_case(line(1:j-1))
         if(trim(keyword) == "sites") then
           read(line(j+1:),*,iostat=ier) j1,j2,j3
           if(ier /= 0) then
             j1=1; j2=2; j3=3
           end if
         else
            write(*,"(a)") " => The keyword Sites followed by three integers is neede here !"
            stop
         end if
      else
         write(itto,1262,advance='no')        ! Indices of Three Sites ?
                                              ! (For Output of Resulting Moments)
         read(itti,*)j1,j2,j3
         write(iba,*)j1,j2,j3

         if((j1 <= 0).or.(j1 > na))j1=1
         if((j2 <= 0).or.(j2 > na))j2=2
         if((j3 <= 0).or.(j3 > na))j3=3
      end if

      if(batch) then
         do
           read(i01,"(a)",iostat=ier) line
           if(ier == 0) then
             write(lpt,"(a)") "    mcm-line => "//trim(line)
             line=adjustl(line)
             if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
             exit
           else
             write(*,"(a)") " => Premature end of file (Schedule): "//trim(nfil)//".mcm"
             stop
           end if
         end do       !123456789
         j=index(line," ")
         keyword=l_case(line(1:j-1))
         if(trim(keyword) == "schedule") then
           read(line(j+1:),*,iostat=ier) t,coef,tfin
           if(ier /= 0) then
            write(*,"(a)") " => Error reading T_ini, Coeff and  T_final !"
            stop
           end if
         else
            write(*,"(a)") " => The keyword Schedule followed by three reals (T_ini, Coeff, T_final) is neede here !"
            stop
         end if
      else
         do
            write(itto,"(a)") ' => Give initial temperature, HEATING (>1) or COOLING (<1) multiplier'
            write(itto,"(a)",advance='no') '    and final temperature (in Kelvin) : '

            !Kirkpatrick's Cooling(Heating) Scheme : T(n+1) = COEF*T(n)

            read(itti,*,iostat=ier)t,coef,tfin
            if(ier /= 0) cycle
            exit
         end do
         write(iba,*) t,coef,tfin
      end if
      !Calculating the expected number of temperatures
      temp=t
      ntemp=0
      if( coef < 1.0) then
         do
           ntemp=ntemp+1
           temp=temp*coef
           if(temp < tfin) exit
         end do
      else
         do
           ntemp=ntemp+1
           temp=temp*coef
           if(temp > tfin) exit
         end do
      end if
      write(lpt,"(a,i5)") "  => Expected number of temperatures: ",ntemp
      iwrt=ntemp
      ansh = 'T'     ! Ansh=T => Iteration On Temperature
      cod  = 'M'     ! Cod=M => Multiplicative Coefficient
      codt = 'x'
      t_glob=0.0
      call cpu_time(t_ini)
      job=0
   do_hfield: do
       call cpu_time(t_part)
       t_glob=t_glob+t_part-t_ini
       cpu=t_part
      if(batch) then
         do
           read(i01,"(a)",iostat=ier) line
           if(ier == 0) then
             write(lpt,"(a)") "    mcm-line => "//trim(line)
             line=adjustl(line)
             if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
             exit
           else
             write(*,"(a)") " => Premature end of file (Hfield): "//trim(nfil)//".mcm"
             stop
           end if
         end do       !123456789
         j=index(line," ")
         keyword=l_case(line(1:j-1))
         if(trim(keyword) == "hfield") then
           read(line(j+1:),*,iostat=ier) hk,hx0,hy0,hz0
           if(ier /= 0) then
            write(*,"(a)") " => Error reading H, hx,hy,hz !"
            stop
           end if
           if( (hk == 0).and.(hx0 == 0).and. (hy0 == 0).and.(hz0 == 0) ) hx0 = 1.0
         else
            write(*,"(a)") " => The keyword Hfield followed by four reals (H-Strength, hx,hy,hz) is neede here !"
            stop
         end if
      else
         do
            write(itto,"(a)",advance='no') ' => Strength and direction of the magnetic field ?: '               ! MAGNETIC FIELD ?
            read(itti,*,iostat=ier)hk,hx0,hy0,hz0
            if(ier /= 0) cycle
            if( (hk /= 0).and.(hx0 == 0).and. (hy0 == 0).and.(hz0 == 0) ) cycle
            if( (hk == 0).and.(hx0 == 0).and. (hy0 == 0).and.(hz0 == 0) ) hx0 = 1.0
            write(iba,*)hk,hx0,hy0,hz0
            exit
         end do
      end if

      if(batch) then
         do
           read(i01,"(a)",iostat=ier) line
           if(ier == 0) then
             write(lpt,"(a)") "    mcm-line => "//trim(line)
             line=adjustl(line)
             if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
             exit
           else
             write(*,"(a)") " => Premature end of file (Mcyc): "//trim(nfil)//".mcm"
             stop
           end if
         end do       !123456789
         j=index(line," ")
         keyword=l_case(line(1:j-1))
         if(trim(keyword) == "mcyc") then
           read(line(j+1:),*,iostat=ier) it,nom
           if(ier /= 0) then
            write(*,"(a)") " => Error reading Number of Montecarlo cycles !"
            stop
           end if
         else
            write(*,"(a)") " => The keyword Mcyc followed by two integers (Num_MCS, Num_Thermal) is neede here !"
            stop
         end if
      else
         do
            write(itto,"(a/a)",advance='no') '   Give : - Total number of MCS/S', &
                                '          - Number of thermalisation MCS/S: '
            read(itti,*,iostat=ier)it,nom
            if(ier /= 0) cycle
            write(iba,*)it,nom
            exit
         end do
      end if

      snom=real(it-nom)

      write(lpt,2112)                ! PARAMETERS FOR CURRENT SCHEME
        2112 FORMAT(8X,  &
            ' _______________________________________________________ '  &
            /8X,'|                                                       |'  &
            /8X,'|   SIMULATION PARAMETERS USED FOR THE FOLLOWING RUN    |')
      write(lpt,2013)it,nom,t,tfin,codt,coef,hk,hk
        2113 FORMAT(8X,  &
            '|_______________________________________________________|'  &
            /8X,'|                                                       |'  &
            /8X,'| at any H :',i6,'MCS/S  with',i6,' omitted for average |'  &
            /8X,'|     From T =',f8.3,'  to T =',f8.3,'                  |'  &
            /8X,'|     Temperature program :        none                 |'  &
            /8X,'|     From H =',f9.2,' to H =',f9.2,'  kOe            |'  &
            /8X,'|     Field program       : H(n+1) = H(n) + ',f8.2,'    |'  &
            /8X,'|     Field in the direction  :  ',3(f6.2,1X),'  |'  &
            /8X,'|_______________________________________________________|')
      if(hk /= 0) then
        write(lpt,2031)hx0,hy0,hz0
      end if
      write(lpt,2030)

    do_print: do
      call cpu_time(t_part)
      t_glob=t_glob+t_part
      job=job+1
      write(itto,"(/a)")    " -----------"
      write(itto,"(a,i4)")  " => NEW JOB: ",job
      write(itto,"(a//)")   " -----------"
      hm0=SQRT(hx0*hx0+hy0*hy0+hz0*hz0)
      hdix=hx0/hm0
      hdiy=hy0/hm0
      hdiz=hz0/hm0
      hi=hk/14.88732                 ! CONVERSION HK(Kelvin)->HI(kOe)
      hx=hdix*hi                     ! HX,HY,HZ: EFFECTIVE COMPONENTS
      hy=hdiy*hi                     ! OF THE MAGNETIC FIELD
      hz=hdiz*hi
      write(lpt,"(/)")

      if(batch) then
         do
           read(i01,"(a)",iostat=ier) line
           if(ier == 0) then
             write(lpt,"(a)") "    mcm-line => "//trim(line)
             line=adjustl(line)
             if(line(1:1) == "!" .or. line(1:1) == "#"  .or. len_trim(line) == 0) cycle
             exit
           else
             write(*,"(a)") " => Premature end of file (Print): "//trim(nfil)//".mcm"
             stop
           end if
         end do       !123456789
         j=index(line," ")
         keyword=l_case(line(1:j-1))
         if(trim(keyword) == "print") then
           read(line(j+1:),*,iostat=ier) ansnev
           if(ier /= 0) ansnev="N"
         else
            write(*,"(a)") " => The keyword Print followed by N, E or A, is neede here !"
            stop
         end if
      else
         do
            write(itto,"(a)") ' => Printout of the configuration of magnetic moments:'
            write(itto,"(a)") '     N = never'
            write(itto,"(a)") '     E = at the final temperature/field only'
            write(itto,"(a)") '     A = at every temperature/field'
            write(itto,"(a)",advance='no') ' => Option: '
            read(itti,"(a)")ansnev
            if((ansnev /= 'A').and.(ansnev /= 'a').and.  &
                (ansnev /= 'E').and.(ansnev /= 'e').and.  &
                (ansnev /= 'N').and.(ansnev /= 'n')) cycle
            exit
         end do
         write(iba,"(a)")ansnev
      end if

      if(batch) then
         do
           read(i01,"(a)",iostat=ier) line
           if(ier == 0) then
             write(lpt,"(a)") "    mcm-line => "//trim(line)
             line=adjustl(line)
             if(line(1:1) == "!" .or. line(1:1) == "#"  .or. len_trim(line) == 0) cycle
             exit
           else
             write(*,"(a)") " => Premature end of file (Averages): "//trim(nfil)//".mcm"
             stop
           end if
         end do       !123456789
         j=index(line," ")
         keyword=l_case(line(1:j-1))
         if(trim(keyword) == "averages") then
           ansrf='Y'
         else
            ansrf='N'
            backspace(unit=i01)
         end if
      else
         do
           write(itto,"(3a)",advance='no') ' => Output of averaged characteristics in file ',trim(nfil)//'.res',' (Y or N)?: '
           read(itti,"(a)") ansrf
           if((ansrf /= 'Y').and.(ansrf /= 'y').and.  &
              (ansrf /= 'N').and.(ansrf /= 'n')) cycle
           exit
         end do
         write(iba,"(a)")ansrf
      end if


      !***** OPENING AND FIRST LINES OF THE AVERAGED CHARARACTERISTICS FILE *****


      if((ansrf == 'Y') .or. (ansrf == 'y')) then
         open(unit=i03,file=trim(nfil)//'.res',status='unknown')
         ou3=1
         write(itto,"(3a)") ' => File ',trim(nfil)//'.res',' created'
         nres=nres+1
         if(nres == 1) then
           write(i03,"(3a)") '[ Input file : ',trim(nfil)//'.mcm ]  ',titl1
           write(i03,"(a)") "   "//titl2
           write(i03,"(a)") "   "//titl
           if(nq /= 0) then
             write(i03,"(4(a,i2),2a)") "   ",nq,spimod//' Spins - Sample size : ',lu,' X',lv,' X',lw,' - results per ',avchar
           else
             write(i03,"(3(a,i2),2a)") "   "//spimod,lu,' X',lv,' X',lw,' - results per ',avchar
           end if
         end if
         write(i03,"(1x,i2,i10,a,i5,a,3(f6.2,2X))")  &
                 nres,it,' (-',nom,' )  MCS/S   with H and/or M// along : ',hx0,hy0,hz0
         write(i03,"(a,3(i2,a))") '       T      H      1/SUS    SP. HT.      EN     MTOT  M(', &
                                  j1,') M(',j2,') M(',j3,')     M//   Fc'
      end if

      !***** Averaged Characteristics File Opened *****

      !----------------------------------------------------------
      do   !Loop over possible different conditions (interactive)
      !-----------------------------------------------------------
         if(hk /= 0) then
           write(lpt,484)t,hk,hi,hx0,hy0,hz0
           write(itto,484)t,hk,hi,hx0,hy0,hz0
           484 format(1X,'***********************',/,' **  T =  ',f8.3,  &
               '    **************************************************',/,  &
               ' **  H =',f9.2,' kOe ( ',f7.2,' [K] )  along : ',3(1X,f6.3),  &
               '  **',/,' ************************************************',  &
               '***********************')
         else
           write(lpt,494)t,hk,hi
           write(itto,494)t,hk,hi
           494 format(1X,'***********************',/,' **  T =  ',f8.3,  &
               '    *******************',/,' **  H =',f9.2,  &
               ' kOe ( ',f7.2,' [K] )  **',/, ' ****************************************')
         end if

         write(itto,"(a)") ' => Starting monte-carlo loops'

         call en_tot(1,energy)          ! Calculate the Total Energy
                                        ! with the Current Magnetic Field
         call mc_cycle()                ! Execute the Monte-Carlo Loops


         !*****    TEST ifTHE CURRENT ITERATION IS THE LAST ONE *****


         if((ansh == 'F').or.(ansh == 'f')) then
           hki=hk+hste
           if((hste > 0).and.(hki > hfin)) last=1
           if((hste < 0).and.(hki < hfin)) last=1

         else
           if((cod == 'M').or.(cod == 'm')) then
             tci=t*coef
             if((coef > 1).and.(tci > tfin)) last=1
             if((coef < 1).and.(tci < tfin)) last=1
           else
             tci=t+coef
             if((coef > 0).and.(tci > tfin)) last=1
             if((coef < 0).and.(tci < tfin)) last=1
           end if

         end if


         !*****    end OF LAST ITERATION TEST *****


         call out_conf()         ! Output Configuration Of Moments

         if(jcod == 0) then     ! Calculate the Constraint Function
           call const_funct()    ! of the System (For Isotropic
         end if                 ! Exchange Only)

         call out_res()          ! Output Averaged Characteristics

         call en_tot(2,energy)   ! Calculate the Total Energy
                                 ! With the Current Magnetic Field
         !*****    INCREMENT FIELD OR TEMPERATURE *****


         if((ansh == 'F').or.(ansh == 'f')) then
                                          ! ANSH=F => ITERATION ON FIELD
             hk=hk+hste                   ! HK = NEW MAGNETIC FIELD
             hi=hk/14.88732
             hx=hdix*hi
             hy=hdiy*hi
             hz=hdiz*hi

             if((hste > 0).and.(hk <= hfin)) cycle
             if((hste < 0).and.(hk >= hfin)) cycle

         else                           ! ANSH=T => ITERATION ON TEMPERATURE
             if((cod == 'M').or.(cod == 'm')) then
                                          ! COD=M => MULTIPLICATIVE COEFFICIENT
                                          ! KIRKPATRICK'S COOLING(HEATING) SCHEME
               ta=t*coef
               if((coef > 1).and.(ta <= tfin)) then
                 t=ta
                 cycle
               end if
               if((coef < 1).and.(ta >= tfin)) then
                 t=ta
                 cycle
               end if
             else                         ! COD=A => ADDITIVE COEFFICIENT
                                          ! LINEAR COOLING(HEATING) SCHEME
               ta=t+coef
               if((coef > 0).and.(ta <= tfin)) then
                 t=ta
                 cycle
               end if
               if((coef < 0).and.(ta >= tfin)) then
                 t=ta
                 cycle
               end if
             end if
         end if
         exit
      !-------------------------
      end do
      !-------------------------

      !***** End of Incrementation *****


      last=0

      if(batch) then
         do
           read(i01,"(a)",iostat=ier) line
           if(ier == 0) then
             write(lpt,"(a)") "    mcm-line => "//trim(line)
             line=adjustl(line)
             if(line(1:1) == "!" .or. line(1:1) == "#"  .or. len_trim(line) == 0) cycle
             exit
           else
             write(*,"(a)") " => Premature end of file (Continue): "//trim(nfil)//".mcm"
             stop
           end if
         end do       !123456789
         j=index(line," ")
         keyword=l_case(line(1:j-1))
         if(trim(keyword) == "continue") then
            ansc='Y'
            line=adjustl(l_case(line(j:)))
            if(line(1:1) == "f")  then
                ansh='F'
            else
                ansh='T'
            end if
         else
            ansc='N'
            backspace(unit=i01)
         end if

      else !batch

         do
            write(itto,"(a)",advance='no') ' => Do you want to continue (y or n) ?: '
            read(itti,"(a)")ansc
            write(lpt,"(a)")
            if((ansc /= 'Y').and.(ansc /= 'y').and.  &
                (ansc /= 'N').and.(ansc /= 'n')) cycle
            write(iba,"(a)")ansc
            exit
         end do
      end if

      !---------------------------------------------------
      if((ansc == 'Y').or.(ansc == 'y')) then   !interactive
      !--------------------------------------------------
          if(batch) then  !after a continue instrucction followed by T
             do
               read(i01,"(a)",iostat=ier) line
               if(ier == 0) then
                 write(lpt,"(a)") "    mcm-line => "//trim(line)
                 line=adjustl(line)
                 if(line(1:1) == "!" .or. line(1:1) == "#"  .or. len_trim(line) == 0) cycle
                 exit
               else
                 write(*,"(a)") " => Premature end of file (tchange/fchange): "//trim(nfil)//".mcm"
                 stop
               end if
             end do
             if(ansh == 'T') then  !Here we want to do a new temperature run
                j=index(l_case(line),"tchange")
                if(j == 0) then
                 write(*,"(a)") " => An instruction 'tchange' is expected after 'continue T' ! "
                 stop
                end if
                read(line(j+7:),*,iostat=ier) cod,coef,tfin
                if(ier /= 0) then
                  write(*,"(a)") " => Error reading the instruction 'tchange':  "//trim(line)
                  stop
                end if
                if((cod == 'M').or.(cod == 'm')) then
                  t=t*coef     ! First Temperature of the New Iteration
                  codt = 'x'
                  !Recalculate the number of temperatures
                  ntemp=0; temp=t
                  if( coef < 1.0) then
                     do
                       ntemp=ntemp+1
                       temp=temp*coef
                       if(temp < tfin) exit
                     end do
                  else
                     do
                       ntemp=ntemp+1
                       temp=temp*coef
                       if(temp > tfin) exit
                     end do
                  end if

                else

                  t=t+coef     ! First Temperature of the New Iteration
                  codt = '+'
                  ntemp=nint((tfin-t)/coef)+1
                end if
                iwrt=ntemp
                cycle do_hfield
             else
                j=index(l_case(line),"fchange")
                if(j == 0) then
                 write(*,"(a)") " => An instruction 'fchange' is expected after 'continue F' ! "
                 stop
                end if
                read(line(j+7:),*,iostat=ier) hx0,hy0,hz0,hsta,hste,hfin
                if(ier /= 0) then
                   write(*,"(a)") " => Error reading the instruction 'fchange':  "//trim(line)
                   stop
                end if
                ntemp=nint(abs(hfin-hsta)/hste)+1
                iwrt=ntemp
                hk=hsta
             end if

          else

             do
                write(itto,"(a)",advance='no') ' => Modify temperature (T) or magnetic field (F) ?: '
                read(itti,"(a)")ansh
                if((ansh /= 'F').and.(ansh /= 'f').and.  &
                    (ansh /= 'T').and.(ansh /= 't')) cycle
                write(iba,"(a)")ansh
                exit
             end do
             if((ansh == 'T').or.(ansh == 't')) then
               do
                  write(itto,"(a)")  ' => Define code for HEATING/COOLING factor :'
                  write(itto,"(a)")  '              M = multiply'
                  write(itto,"(a)")  '              A = add'
                  write(itto,"(a)",advance='no')  ' => Option: '
                  read(itti,"(a)")cod
                  if((cod /= 'M').and.(cod /= 'm').and.  &
                      (cod /= 'A').and.(cod /= 'a')) cycle
                  write(iba,"(a)")cod
                  exit
               end do
               do
                 write(itto,"(a)") ' => Heating/cooling factor and final temperature ?: '

                 !   Kirkpatrick's Cooling(Heating) Scheme : T(n+1) = COEF*T(n)
                 !   Linear Cooling(Heating) Scheme : T(n+1) = COEF+T(n)

                 read(itti,*,iostat=ier)coef,tfin
                 if(ier /= 0) cycle
                 write(iba,*)coef,tfin
                 exit
               end do

               if((cod == 'M').or.(cod == 'm')) then
                 t=t*coef     ! First temperature of the new iteration
                 codt = 'x'

               else
                 t=t+coef     ! First temperature of the new iteration
                 codt = '+'
               end if
               cycle do_hfield
             end if
          end if

          if(.not. batch) then
             if ( ansh == 'F') then
                do
                   write(itto,"(a)",advance='no') ' => Orientation of the magnetic field h ?: '
                   read(itti,*,iostat=ier) hx0,hy0,hz0
                   if( (hx0 == 0) .and. (hy0 == 0).and.(hz0 == 0) .or.  ier /=0) cycle
                   if(.not. batch) write(iba,*) hx0,hy0,hz0
                   exit
                end do

                do
                   write(itto,"(a)",advance='no')   ' => Starting, increment and final fields ?: '
                   read(itti,*,iostat=ier)hsta,hste,hfin
                   if(ier /= 0) cycle
                   write(iba,*)hsta,hste,hfin
                   hk=hsta
                   exit
                end do
             end if
          end if

          if(batch) then
             do
               read(i01,"(a)",iostat=ier) line
               if(ier == 0) then
                 write(lpt,"(a)") "    mcm-line => "//trim(line)
                 line=adjustl(line)
                 if(line(1:1) == "!" .or. line(1:1) == "#" .or. len_trim(line) == 0) cycle
                 exit
               else
                 write(*,"(a)") " => Premature end of file (Mcyc): "//trim(nfil)//".mcm"
                 stop
               end if
             end do       !123456789
             j=index(line," ")
             keyword=l_case(line(1:j-1))
             if(trim(keyword) == "mcyc") then
               read(line(j+1:),*,iostat=ier) it,nom
               if(ier /= 0) then
                write(*,"(a)") " => Error reading Number of Montecarlo cycles !"
                stop
               end if
             else
                write(*,"(a)") " => The keyword Mcyc followed by two integers (Num_MCS, Num_Thermal) is neede here !"
                stop
             end if
             snom=real(it-nom)
          else
             do
                write(itto,"(a/a)",advance='no') '   Give : - Total number of MCS/S', &
                                    '          - Number of thermalisation MCS/S: '
                read(itti,*,iostat=ier)it,nom
                if(ier /= 0) cycle
                write(iba,*)it,nom
                exit
             end do
             snom=real(it-nom)
          end if

          write(lpt,2112)              ! Parameters for Current Scheme
          write(lpt,2113)it,nom,t,t,hsta,hfin,hste,hx0,hy0,hz0
          cycle do_print
      end if  !         if((ansc == 'Y').or.(ansc == 'y')) then or continue
      exit
    end do do_print
    exit
   end do do_hfield

      if((ansrf == 'Y').or.(ansrf == 'y')) then
        write(i03,509)
      end if

      if(batch) then
         do
           read(i01,"(a)",iostat=ier) line
           if(ier == 0) then
             write(lpt,"(a)") "    mcm-line => "//trim(line)
             line=adjustl(line)
             if(line(1:1) == "!" .or. line(1:1) == "#"  .or. len_trim(line) == 0) cycle
             exit
           else
             write(*,"(a)") " => Premature end of file (Cryst): "//trim(nfil)//".mcm"
             stop
           end if
         end do       !123456789
         j=index(line," ")
         keyword=l_case(line(1:j-1))
         if(trim(keyword) == "cryst") then
            ansm='Y'
            read(unit=line(j+1:),fmt=*,iostat=ier)  nor,dorx,dory,dorz
            if(ier /= 0) then
              write(*,"(a)") " => Error reading site and orienting direction after keyword CRYST ! "
              stop
            end if
         else
            ansm='N'
            backspace(unit=i01)
         end if
      else
          do
             write(itto,"(a)") ' => Do you want a printout of the coomponents of magnetic moments'
             write(itto,"(a)",advance='no') '    in the Crystallographic System of Coordinates ?: '
             read(itti,"(a)")ansm
             if((ansm /= 'Y').and.(ansm /= 'y').and.  &
                 (ansm /= 'N').and.(ansm /= 'n'))cycle
             exit
          end do
          write(iba,"(a)")ansm
      end if
        if((ansm == 'Y').or.(ansm == 'y')) then
          call pro_rota()              ! Projection and Rotation of
          rotated=.true.               ! Moments Inside the Crystal
                                       ! Coordinates System
        end if
!***** OUTPUT OF THE LAST GENERATED SPIN CONFIGURATION (ALSO IN FST format) *****


        OPEN(UNIT=i04,STATUS='unknown',FILE=trim(nfil)//'.spi' )
        write(i04,"(a)")titl1
        write(i04,"(a)")titl2

        if(nq /= 0) then
          write(i04,"(i3,a)")nq," "//titl3
        else
          write(i04,"(a)")titl3
        end if

        DO w=1,lw
          DO v=1,lv
            DO u=1,lu
              write(i04,798)
              write(i04,*)u,v,w
              write(i04,799)
              DO n=1,na
                write(i04,*)n,sx(n,u,v,w),sy(n,u,v,w),sz(n,u,v,w)
              end DO
            end DO
          end DO
        end DO
        close(unit=i04)
        if(coordinates_read) then
           OPEN(UNIT=i04,STATUS='replace',FILE=trim(nfil)//'.fst' )
           write(i04,"(a)")"!  File generated by MCMAG"
           write(i04,"(a)")"TITLE "//trim(titl1)//"  "//trim(titl2)
           write(i04,"(a)")"SPACEG P 1 "
           write(i04,"(a,3f12.6,3f12.4)")"CELL ",lu*aax,lv*aay,lw*aaz,aaa,aab,aag
           write(i04,"(a)")"  "
           write(i04,"(a)") 'BOX   -0.15  1.15   -0.15  1.15    -0.15  1.15'
           DO w=1,lw
             DO v=1,lv
               DO u=1,lu
                 DO n=1,na
                   atm_text=" "
                   write(atm_text,"(a,3i2.2)") trim(namt(n))//"_",u-1,v-1,w-1
                   pos=(real([u-1,v-1,w-1])+xyz(:,n))/real([lu,lv,lw])
                   write(i04,"(a,3f10.5,a)") "Atom "//trim(atm_text)//"  "//elem(n),pos
                 end DO
               end DO
             end DO
           end DO
           write(i04,"(a)")"  "
           write(i04,"(a)")'Mag_Structure'
           write(i04,"(a)")'Lattice P'
           write(i04,"(a)")'Kvect    0.00   0.00   0.00'
           write(i04,"(a)")'SYMM   x, y, z'
           write(i04,"(a)")'MSYM   u, v, w, 0.00'
           DO w=1,lw
             DO v=1,lv
               DO u=1,lu
                 DO n=1,na
                   atm_text=" "
                   spv=[sx(n,u,v,w),sy(n,u,v,w),sz(n,u,v,w)] !Spins of the last configuration in Cartesian Coordinates
                   spr=matmul(ac,spv)                        !Converted to unitary system along a,b,c
                   write(atm_text,"(a,3i2.2)") trim(namt(n))//"_",u-1,v-1,w-1
                   pos=(real([u-1,v-1,w-1])+xyz(:,n))/real([lu,lv,lw])
                   write(i04,"(a,3f10.5,a)") "Matom "//trim(atm_text)//"  "//elem(n),pos,'  SCALE 1.0 GROUP'
                   write(i04,"(a20,3f10.5,a)") "SkP    1   1 ",spr,'  0.00  0.00  0.00    0.00  '
                 end DO
               end DO
             end DO
           end DO
           write(i04,"(a)")'End_Mag_Structure'
           close(unit=i04)

           if(rotated) then
             !write another fst-file with rotated moments
             OPEN(UNIT=i04,STATUS='replace',FILE=trim(nfil)//'.cfl' )
             write(i04,"(a,i3,a,3f8.3,a)")"!  File generated by MCMAG: Moments rotated by aligning moment: ",&
                                         nor, " along [",dorix,doriy,doriz," ]"
             write(i04,"(a)")"Title "//trim(titl1)//"  "//trim(titl2)
             write(i04,"(a)")"Spaceg P 1 "
             write(i04,"(a,3f12.6,3f12.4)")"Cell ",lu*aax,lv*aay,lw*aaz,aaa,aab,aag
             write(i04,"(a)")"  "
             write(i04,"(a)") 'Box   -0.15  1.15   -0.15  1.15    -0.15  1.15'
             write(i04,"(a)")"  "
             write(i04,"(a)")'Mag_Structure'
             write(i04,"(a)")'Lattice P'
             write(i04,"(a)")'Kvect    0.00   0.00   0.00'
             write(i04,"(a)")'Symm   x, y, z'
             write(i04,"(a)")'Msym   u, v, w, 0.00'
             DO w=1,lw
               DO v=1,lv
                 DO u=1,lu
                   DO n=1,na
                     atm_text=" "  !(COMX,SOMY,SOMZ)<- output of pro_rota
                     if(nor == 0) then
                        spr=[comx(n,u,v,w),somy(n,u,v,w),somz(n,u,v,w)] !Average Spins in Crystallographic Coordinates
                     else
                        spr=[somrx(n,u,v,w),somry(n,u,v,w),somrz(n,u,v,w)] !Average Spins in Crystallographic Coordinates after rotation
                     end if
                     write(atm_text,"(a,3i2.2)") trim(namt(n))//"_",u-1,v-1,w-1
                     pos=(real([u-1,v-1,w-1])+xyz(:,n))/real([lu,lv,lw])
                     write(i04,"(a,3f10.5,a)") "Matom "//trim(atm_text)//"  "//scattf(n),pos,'  SCALE 1.0 GROUP'
                     write(i04,"(a20,3f10.5,a)") "SkP    1   1 ",spr,'  0.00  0.00  0.00    0.00  '
                     spv=matmul(aa,spr)  !Cartesian components
                     sx(n,u,v,w)=spv(1)  !Copy the average Cartesian moments into spin variables
                     sy(n,u,v,w)=spv(2)
                     sz(n,u,v,w)=spv(3)
                     !write(*,"(4i4,3f10.5,tr5,3f10.5)") n,u,v,w,spr,spv
                   end DO
                 end DO
               end DO
             end DO
             write(i04,"(a)")'End_Mag_Structure'
             close(unit=i04)
           else    !!Copy the average moments into spin variables even without rotation
             DO w=1,lw
               DO v=1,lv
                 DO u=1,lu
                   DO n=1,na
                     spr=[comx(n,u,v,w),somy(n,u,v,w),somz(n,u,v,w)] !Moments of the last cycle in Crystallographic Coordinates
                     spv=matmul(aa,spr)  !Cartesian components
                     sx(n,u,v,w)=spv(1)  !Copy the average moments into spin variables
                     sy(n,u,v,w)=spv(2)  !Before calculating the total energy
                     sz(n,u,v,w)=spv(3)
                   end DO
                 end DO
               end DO
             end DO
           end if!Rotated
           call en_tot(3,e_aver)
           delta=e_aver-energy

           write(lpt,"(a/a,f12.5/)") " => Difference betwen average energy and the energy", &
                          "    of the last spin configuration:",delta
           write(*,"(a/a,f12.5/)") " => Difference betwen average energy and the energy", &
                          "    of the last spin configuration:",delta
        end if

! ***** OUTPUT COMMENTS AT THE end OF THE PROGRAM *****
      call cpu_time(t_fin)
      write(unit=*,fmt="(a,f12.4,a)")  " => CPU time: ",t_fin-t_ini, " seconds"


      write(itto,"(a)") " => The following files have been created: "
      write(itto,"(a)") "    "//trim(nfil)//'.rec    with the sequence of instruction you gave'
      write(itto,"(a)") "    "//trim(nfil)//'.lis    with all the results of simulation'
      write(itto,"(a)") "    "//trim(nfil)//'.spi    with the last generated spin configuration (Cartesian)'
      if(coordinates_read) then
         write(itto,"(a)") "    "//trim(nfil)//'.fst    with the last generated spin configuration (Crystallographic)'
        if(rotated) write(itto,"(a)") "    "//trim(nfil)//'.cfl    with the average moments after rotations (Crystallographic)'
      end if

      write(lpt,"(a)") " => The following files have been created: "
      write(lpt,"(a)") "    "//trim(nfil)//'.rec    with the sequence of instruction you gave'
      write(lpt,"(a)") "    "//trim(nfil)//'.lis    with all the results of simulation'
      write(lpt,"(a)") "    "//trim(nfil)//'.spi    with the last generated spin configuration (Cartesian)'
      if(coordinates_read) then
         write(lpt,"(a)") "    "//trim(nfil)//'.fst    with the last generated spin configuration (Crystallographic)'
        if(rotated) write(lpt,"(a)") "    "//trim(nfil)//'.cfl    with the average moments after rotations (Crystallographic)'
      end if

      if(ou3 /= 0) then
        write(lpt,"(a)")  "    "//trim(nfil)//'.res    with condensed list of averaged characteristics'
        write(itto,"(a)") "    "//trim(nfil)//'.res    with condensed list of averaged characteristics'
      end if

      write(lpt,597)                 ! end COMMENTS
      STOP


      2 FORMAT(a6)
      2003 FORMAT(1X,' ------------------------------------------',  &
          '----------------------------------',// ,12X,a50,//,  &
          1X,' ------------------------------------------',  &
          '----------------------------------')
      4003 FORMAT(a6,/,'Y',/,i1)
      2222 FORMAT(a80/a80/a80)
      2022 FORMAT(2X,a80)
      888 FORMAT(' => Sample size must be greater than or equal to 1 along any axis')
        708 FORMAT(i1)
        889 FORMAT(i2)
        1262 FORMAT(' => Give the indices of 3 sites whose moments will be collected: ' )
        2015 FORMAT(10X,'  _________________________________________________'  &
            ,/,11X,'|                                                 |'  &
            ,/,11X,'|        GENERAL PARAMETERS OF SIMULATION         |'  &
            ,/,11X,'|_________________________________________________|'  &
            ,/,11X,'|                                                 |'  &
            ,/,11X,'|  SAMPLE SIZE    :    ',i2,' x',i2,' x',i2,'  CELLS'  &
            ,10X,'|',/,11X,'|  with  ',i2,' SITES per CELL and ',i2,' MOLES per CELL  |'  &
            ,/,11X,'|  AVERAGED CHARACTERISTICS   :    per ',a9,  &
            '  |',/,11X,'|  BOUNDARY CONDITIONS        :     ',a10,'    |')
        2014 FORMAT(11X,'|  SPIN MODEL     :     ',i2,a12,'            |'  &
            ,/,11X,'|_________________________________________________|'//)
        2016 FORMAT(11X,'|  SPIN MODEL     :        ',a12,'           |'  &
            ,/,11X,'|_________________________________________________|'//)
        2013 FORMAT(8X,  &
            '|_______________________________________________________|'  &
            /8X,'|                                                       |'  &
            /8X,'| at every T :',i6,'MCS/S  (',i6,' omitted for average) |'  &
            /8X,'|     From T =',f8.3,'  to T =',f8.3,'                  |'  &
            /8X,'|     Temperature program : T(n+1) = T(n) ',a1,f8.3,'     |'  &
            /8X,'|     From H =',f9.2,' to H =',f9.2,'  kOe            |'  &
            /8X,'|     Field program       :        none                 |')
        2031 FORMAT(8X, '|     Field in the direction  :  ',3(f6.2,1X),'  |')
        2030 FORMAT(8X, '|_______________________________________________________|')
        2018 FORMAT(3X,i3,a77)
        1018 FORMAT(1X,'!! Temperature cannot be less than or eq. to zero !!')
          509 FORMAT(' 99')
            798 FORMAT(1X,'CELL')
            799 FORMAT(9X,'Site',5X,'Sx',13X,'Sy',13X,'Sz')
            3000 FORMAT(1X,' ------------------------------------------'  &
                ,'----------------------------------')
            596 FORMAT(  '  ****************************** START SIMULATION *******************************',//)
          597 FORMAT(///,'  ************************************ end **************************************')
          595 FORMAT(1X)
 End Program MCMAG
