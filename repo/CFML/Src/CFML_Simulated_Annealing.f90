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
!!---- MODULE: CFML_Simulated_Annealing
!!----   INFO: Module for Global Optimization using Simulated Annealing.
!!----         Currently there is available only a generic Simulated Anneling subroutine
!!----         That must be called with the name of a user-supplied subroutine to calculate
!!----         the cost function as an argument.
!!----         The calling program must define at least two variables of derived types
!!----         SIMANN_CONDITIONS_TYPE and STATE_VECTOR_TYPE respectively.
!!----         The generic simulated annealing procedure can use the constant step algorithm
!!----         or the Corana algorithm depending on the values of the corresponding component
!!----         of the SIMANN_CONDITIONS_TYPE user-defined variable.
!!----
!!---- DEPENDENCIES
!!--++     Use CFML_GlobalDeps,       only: Cp, Err_CFML, clear_error
!!--++     use CFML_Messages,         mess => write_scroll_text
!!--++     use CFML_Strings,          only: u_case, File_Type
!!----
!!---- HISTORY
!!----    Update: 18/06/2020
!!----
!!----
!!---- VARIABLES
!!--..    Parameters
!!----    NP_CONF
!!----    NP_SAN
!!--..
!!--..    Types
!!----    MULTISTATE_VECTOR_TYPE
!!----    SIMANN_CONDITIONS_TYPE
!!----    STATE_VECTOR_TYPE
!!--..
!!----
!!---- PROCEDURES
!!----    Functions:
!!----
!!----    Subroutines:
!!--++       BOUNDARY_COND               [Private]
!!--++       CHECK                       [Private]
!!--++       CHECKM                      [Private]
!!--++       INIT_RAN                    [Private]
!!----       LOCAL_OPTIM
!!----       SANN_OPT_MULTICONF
!!----       SET_SIMANN_COND
!!----       SET_SIMANN_MSTATEV
!!----       SET_SIMANN_STATEV
!!----       SIMANNEAL_GEN
!!----       SIMANNEAL_MULTICONF
!!----       WRITE_SIMANN_COND
!!----       WRITE_SIMANN_MSTATEV
!!----       WRITE_SIMANN_STATEV
!!----
!!
 Module CFML_Simulated_Annealing
    !---- Use Files ----!
    Use CFML_GlobalDeps,       only: Cp, Err_CFML, clear_error
    use CFML_Messages,         mess => write_scroll_text
    use CFML_Strings,          only: u_case, File_Type, file_list_type

    !---- Variables ----!
    implicit none

    private

    !---- List of public subroutines ----!
    public :: SimAnneal_gen, SimAnneal_MultiConf, Set_SimAnn_Cond, Set_SimAnn_StateV, &
              Write_SimAnn_Cond, Write_SimAnn_StateV, Set_SimAnn_MStateV, Write_SimAnn_MStateV, &
              SAnn_Opt_MultiConf, Local_Optim

    !---- List of private subroutines ----!
    private:: checkm, check, init_ran

    !---- Definitions ----!
    !!----
    !!---- NP_CONF
    !!----    integer, parameter, public :: NP_CONF=30
    !!----
    !!----    Maximum number of initial configurations in paralell
    !!----
    !!---- Update: February - 2005
    !!
    integer, parameter, public  :: np_CONF=30

    !!----
    !!---- NP_SAN
    !!----    integer, parameter, public :: NP_SAN=80
    !!----
    !!----    Maximum number of parameters in the model
    !!----
    !!---- Update: February - 2005
    !!
    integer, parameter, public  :: np_SAN=1024

    !!----
    !!----  TYPE :: MULTISTATE_VECTOR_TYPE
    !!--..
    !!----  Type, public  :: MultiState_Vector_Type
    !!----    integer                                     :: npar      ! Number of parameters of the model. To be supplied in the calling program
    !!----    integer                                     :: nconf     ! Number of configurations.
    !!----    integer,          dimension(np_SAN)         :: code      !=0 fixed parameter, =1 variable parameter
    !!----    integer,          dimension(np_SAN)         :: bound     !=0 fixed boundaries, =1 periodic boundaries
    !!----    real(kind=cp),    dimension(np_SAN,np_CONF) :: state     !Vector State characterizing the current configuration
    !!----    real(kind=cp),    dimension(np_SAN,np_CONF) :: stp       !Step vector (one value for each parameter)
    !!----    real(kind=cp),    dimension(np_CONF)        :: cost      !Vector with cost of the different configurations
    !!----    real(kind=cp),    dimension(np_SAN)         :: low       !Low-limit value of parameters
    !!----    real(kind=cp),    dimension(np_SAN)         :: high      !High-limit value of parameters
    !!----    real(kind=cp),    dimension(np_SAN)         :: config    !Vector State characterizing the current best configuration
    !!----    real(kind=cp),    dimension(np_SAN)         :: sigma     !Sigma of the Vector State characterizing the current best configuration
    !!----    character(len=15),dimension(np_SAN)         :: nampar    !name of parameters of the model
    !!----    real(kind=cp)                               :: best_cost !Cost of the best configurations
    !!----  End Type MultiState_Vector_Type
    !!----
    !!---- Derived type containing the parameters and configurations to be optimized,
    !!---- the limits, steps, names and best configuration to be searched
    !!-----by Simulated Annealing Algorithm
    !!----
    !!---- Update: March - 2005
    !!
    Type, public  :: MultiState_Vector_Type
      integer                                     :: npar      !> Number of parameters of the model. To be supplied in the calling program
      integer                                     :: nconf     !> Number of configurations.
      integer,          dimension(np_SAN)         :: code      !> =0 fixed parameter, =1 variable parameter
      integer,          dimension(np_SAN)         :: bound     !> =0 fixed boundaries, =1 periodic boundaries
      real(kind=cp),    dimension(np_SAN,np_CONF) :: state     !> Vector State characterizing the current configuration
      real(kind=cp),    dimension(np_SAN,np_CONF) :: stp       !> Step vector (one value for each parameter)
      real(kind=cp),    dimension(np_CONF)        :: cost      !> Vector with cost of the different configurations
      real(kind=cp),    dimension(np_SAN)         :: low       !> Low-limit value of parameters
      real(kind=cp),    dimension(np_SAN)         :: high      !> High-limit value of parameters
      real(kind=cp),    dimension(np_SAN)         :: config    !> Vector State characterizing the current best configuration
      real(kind=cp),    dimension(np_SAN)         :: sigma     !> Sigma of the Vector State characterizing the current best configuration
      character(len=15),dimension(np_SAN)         :: nampar    !> name of parameters of the model
      real(kind=cp)                               :: best_cost !> Cost of the best configurations
    End Type MultiState_Vector_Type

    !!----
    !!----  TYPE :: SIMANN_CONDITIONS_TYPE
    !!--..
    !!---- Type, public       :: SimAnn_Conditions_type
    !!----  real(kind=cp)     :: t_ini                 !> Initial temperature
    !!----  real(kind=cp)     :: anneal                !> Kirpactrick factor for Annealing
    !!----  real(kind=cp)     :: accept                !> Minimum percentage of accepted configurations
    !!----  real(kind=cp)     :: threshold             !> Good solutions have cost values below this (used in Sann_Opt_MultiConf)
    !!----  integer           :: initconfig            !> Flag determining if the first configuration is random or read
    !!----  integer           :: nalgor                !> Flag determining if the Corana algorithm is selected (0) or not (/=0)
    !!----  integer           :: nm_cycl               !> Number of Cycles per temp  in SA searchs
    !!----  integer           :: num_temps             !> Maximum number of temperatures in SA
    !!----  integer           :: num_therm             !> Number of thermalization cycles in SA
    !!----  integer           :: num_conf              !> Number of paralell configurations in SA
    !!----  integer           :: seed=0                !> If different from zero, holds the seed
    !!----                                             !> for random number generator
    !!----  character(len=60) :: Cost_function_name
    !!---- End type Opt_Conditions_Type
    !!----
    !!---- Derived type containing the conditions for running the
    !!---- Simulated Annealing Algorithm
    !!----
    !!---- Update: March - 2005
    !!
    Type, public        :: SimAnn_Conditions_type
      real(kind=cp)     :: t_ini       !> Initial temperature
      real(kind=cp)     :: anneal      !> Kirpactrick factor for Annealing
      real(kind=cp)     :: accept      !> Minimum percentage of accepted configurations
      real(kind=cp)     :: threshold   !> Good solutions have cost values below this (used in Sann_Opt_MultiConf)
      integer           :: initconfig  !> Flag determining if the first configuration is random or read
      integer           :: nalgor      !> Flag determining if the Corana algorithm is selected (0) or not (/=0)
      integer           :: nm_cycl     !> Number of Cycles per temp  in SA searchs
      integer           :: num_temps   !> Maximum number of temperatures in SA
      integer           :: num_therm   !> Number of thermalization cycles in SA
      integer           :: num_conf    !> Number of paralell configurations in SA
      integer           :: seed        !> If different from zero, holds the seed for random number generator
      character(len=60) :: Cost_function_name
    End type SimAnn_Conditions_type

    !!----
    !!----  TYPE :: STATE_VECTOR_TYPE
    !!--..
    !!----  Type, public  :: State_Vector_Type
    !!----    integer                    :: npar             !> Number of parameters of the model.
    !!----    integer, dimension(np_SAN) :: code             !> =0 fixed parameter, =1 variable parameter
    !!----    integer, dimension(np_SAN) :: bound            !> =0 fixed boundaries,=1 periodic boundaries
    !!----    real(kind=cp),    dimension(np_SAN) :: state   !> Vector State with the current configuration
    !!----    real(kind=cp),    dimension(np_SAN) :: stp     !> Step vector (one value for each parameter)
    !!----    real(kind=cp),    dimension(np_SAN) :: low     !> Low-limit value of parameters
    !!----    real(kind=cp),    dimension(np_SAN) :: high    !> High-limit value of parameters
    !!----    real(kind=cp),    dimension(np_SAN) :: config  !> Vector State with the best configuration
    !!----    real(kind=cp)                       :: cost    !> Cost of the best configuration
    !!----    character(len=15),dimension(np_SAN) :: nampar  !> name of parameters of the model
    !!----  End Type State_Vector_Type
    !!----
    !!---- Derived type containing the parameters to be optimized,
    !!---- the limits, steps, names and best configuration to be searched
    !!-----by Simulated Annealing Algorithm
    !!----
    !!---- Update: March - 2005
    !!
    Type, public  :: State_Vector_Type
      integer                             :: npar    !> Number of parameters of the model. To be supplied in the calling program
      integer,          dimension(np_SAN) :: code    !> =0 fixed parameter, =1 variable parameter
      integer,          dimension(np_SAN) :: bound   !> =0 fixed boundaries, =1 periodic boundaries
      real(kind=cp),    dimension(np_SAN) :: state   !> Vector State characterizing the current configuration
      real(kind=cp),    dimension(np_SAN) :: stp     !> Step vector (one value for each parameter)
      real(kind=cp),    dimension(np_SAN) :: low     !> Low-limit value of parameters
      real(kind=cp),    dimension(np_SAN) :: high    !> High-limit value of parameters
      real(kind=cp),    dimension(np_SAN) :: config  !> Vector State characterizing the current best configuration
      real(kind=cp)                       :: cost    !> Cost of the best configuration
      character(len=15),dimension(np_SAN) :: nampar  !> name of parameters of the model
    End Type State_Vector_Type

    Interface Set_SimAnn_Cond
       Module Procedure Set_SimAnn_Cond_FT
       Module Procedure Set_SimAnn_Cond_FLT
    End Interface Set_SimAnn_Cond

    Interface

      Module Subroutine Boundary_Cond(n,x,low,high,bound)
         !---- Arguments ----!
         integer,                     intent(in)     :: n
         real(kind=cp), dimension(:), intent(in out) :: x
         real(kind=cp), dimension(:), intent(in)     :: low,high
         integer, dimension(:),       intent(in)     :: bound
      End Subroutine Boundary_Cond

      Module Subroutine Check(c,vs)
         !---- Arguments ----!
         type(SimAnn_Conditions_type), intent(in out) :: c
         type(State_Vector_Type),      intent(in out) :: vs
      End Subroutine Check

      Module Subroutine Checkm(c,vs)
         !---- Arguments ----!
         type(SimAnn_Conditions_type), intent(in out) :: c
         type(MultiState_Vector_Type), intent(in out) :: vs
      End Subroutine Checkm

      Module Subroutine Init_Ran(Seed)
         !---- Argument ----!
         integer, dimension(:), intent (in) :: seed
      End Subroutine Init_Ran

      Module Subroutine Local_Optim(Model_Functn,n,x,f,low,high,bound)
         !---- Arguments ----!
         integer,                      intent(in)      :: n
         real(kind=cp), dimension(:),  intent(in out)  :: x
         real(kind=cp),                intent(out)     :: f
         real(kind=cp), dimension(:),  intent(in)      :: low,high
         integer, dimension(:),        intent(in)      :: bound

         Interface
            Subroutine Model_Functn(v,cost)
               Use CFML_GlobalDeps, only: Cp
               real(kind=cp),dimension(:),    intent( in):: v
               real(kind=cp),                 intent(out):: cost
            End Subroutine Model_Functn
         End Interface
      End Subroutine Local_Optim

      Module Subroutine SAnn_Opt_MultiConf(Model_Funct,c,vs,Ipr,fileSav,fst)
         !---- Arguments ----!
         type(SimAnn_Conditions_type),  intent(in out)  :: c
         type(MultiState_Vector_Type),  intent(in out)  :: vs
         integer,                       intent(in)      :: Ipr
         character(len=*), optional,    intent(in)      :: filesav
         character(len=*), optional,    intent(in)      :: fst

         Interface
            Subroutine Model_Funct(v,cost)
               Use CFML_GlobalDeps, only: Cp
               real(kind=cp),dimension(:),    intent( in):: v
               real(kind=cp),                 intent(out):: cost
            End Subroutine Model_Funct

            Subroutine Write_FST(fst_file,v,cost)
               Use CFML_GlobalDeps, only: Cp
               character(len=*),              intent(in):: fst_file
               real(kind=cp),dimension(:),    intent(in):: v
               real(kind=cp),                 intent(in):: cost
            End Subroutine Write_FST
         End Interface
      End Subroutine SAnn_Opt_MultiConf

      Module Subroutine Set_SimAnn_Cond_FT(file_list,c)
         !---- Arguments ----!
         type(File_Type),             intent( in)  :: file_list
         type(SimAnn_Conditions_type),intent(out)  :: c
      End Subroutine Set_SimAnn_Cond_FT

      Module Subroutine Set_SimAnn_Cond_FLT(file_list,c)
         !---- Arguments ----!
         type(File_List_Type),        intent( in)  :: file_list
         type(SimAnn_Conditions_type),intent(out)  :: c
      End Subroutine Set_SimAnn_Cond_FLT

      Module Subroutine Set_SimAnn_MStateV(n,nsol,Con,Bounds,VNam,Vec,vs,cod)
         !---- Arguments ----!
         integer,                                     intent(in) :: n,nsol
         integer,                      dimension(:),  intent(in) :: con
         real(kind=cp),                dimension(:,:),intent(in) :: Bounds
         character(len=*),             dimension(:),  intent(in) :: VNam
         real(kind=cp),                dimension(:),  intent(in) :: Vec
         type(MultiState_Vector_Type),                intent(out):: vs
         integer, optional,            dimension(:),  intent(in) :: cod
      End Subroutine Set_SimAnn_MStateV

      Module Subroutine Set_SimAnn_StateV(n,Con,Bounds,VNam,Vec,vs,cod)
         !---- Arguments ----!
         integer,                                intent(in) :: n
         integer,                 dimension(:),  intent(in) :: con
         real(kind=cp),           dimension(:,:),intent(in) :: Bounds
         character(len=*),        dimension(:),  intent(in) :: VNam
         real(kind=cp),           dimension(:),  intent(in) :: Vec
         type(State_Vector_Type),                intent(out):: vs
         integer, optional,       dimension(:),  intent(in) :: cod
      End Subroutine Set_SimAnn_StateV

      Module Subroutine SimAnneal_Gen(Model_Funct,c,vs,Ipr,fileSav,fst)
         !---- Arguments ----!
         type(SimAnn_Conditions_type),intent(in out)  :: c
         type(State_Vector_Type),     intent(in out)  :: vs
         integer,                     intent(in)      :: Ipr
         character(len=*), optional,  intent(in)      :: filesav
         character(len=*), optional,  intent(in)      :: fst

         Interface
            Subroutine Model_Funct(v,cost)
               Use CFML_GlobalDeps, only: Cp
               real(kind=cp),dimension(:),    intent( in):: v
               real(kind=cp),                 intent(out):: cost
            End Subroutine Model_Funct

            Subroutine Write_FST(fst_file,v,cost)
               Use CFML_GlobalDeps, only: Cp
               character(len=*),            intent(in):: fst_file
               real(kind=cp),dimension(:),  intent(in):: v
               real(kind=cp),               intent(in):: cost
            End Subroutine Write_FST
         End Interface
      End Subroutine SimAnneal_Gen

      Module Subroutine SimAnneal_MultiConf(Model_Funct,c,vs,Ipr,fileSav,fst)
         !---- Arguments ----!
         type(SimAnn_Conditions_type),  intent(in out)  :: c
         type(MultiState_Vector_Type),  intent(in out)  :: vs
         integer,                       intent(in)      :: Ipr
         character(len=*), optional,    intent(in)      :: filesav
         character(len=*), optional,    intent(in)      :: fst

         Interface
            Subroutine Model_Funct(v,cost)
               Use CFML_GlobalDeps, only: Cp
               real(kind=cp),dimension(:), intent( in):: v
               real(kind=cp),              intent(out):: cost
            End Subroutine Model_Funct

            Subroutine Write_FST(fst_file,v,cost)
               Use CFML_GlobalDeps,       only: Cp
               character(len=*),           intent(in):: fst_file
               real(kind=cp),dimension(:), intent(in):: v
               real(kind=cp),              intent(in):: cost
            End Subroutine Write_FST
         End Interface
      End Subroutine SimAnneal_MultiConf


      Module Subroutine Write_SimAnn_Cond(ipr,c)
         !---- Arguments ----!
         integer,                     intent(in)  :: ipr
         type(SimAnn_Conditions_type),intent(in)  :: c
      End Subroutine Write_SimAnn_Cond

      Module Subroutine Write_SimAnn_MStateV(ipr,vs,text,cost)
         !---- Arguments ----!
         integer,                     intent(in) :: ipr
         type(MultiState_Vector_Type),intent(in) :: vs
         character(len=*),            intent(in) :: text
         integer, optional,           intent(in) :: cost
      End Subroutine Write_SimAnn_MStateV

      Module Subroutine Write_SimAnn_StateV(ipr,vs,text)
         !---- Arguments ----!
         integer,                intent(in)  :: ipr
         type(State_Vector_Type),intent(in)  :: vs
         character(len=*),       intent(in)  :: text
      End Subroutine Write_SimAnn_StateV

    End Interface

 Contains

 End Module CFML_Simulated_Annealing
