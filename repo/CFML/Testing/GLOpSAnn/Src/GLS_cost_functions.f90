  !!----  Module cost_functions
  !!----
  !!----  Module containing the model functions to be used in a Simulated Annealing procedure
  !!----  The module uses many others from CrysFML (see the 'use' statements below).
  !!----  The public subroutines available in this module are the following:
  !!----    General_cost_function   : updates state vectors and calculates the global const function
  !!----    Readn_Set_CostFunctPars : reads in the CFL file
  !!----    Write_CostFunctPars     : write the cost functions parameters
  !!----    Write_FinalCost         : write the final cost
  !!----
  !!----  The currently availabe cost functions are:
  !!----  "F2obs-F2cal"     Cost(F2obs-F2cal)    : Optimization of C0 =Sum|F2obs-F2cal|/Sum|F2obs|
  !!----  "Fobs-Fcal"       Cost(Fobs-Fcal)      : Optimization of C1 =Sum|Fobs-Fcal|/Sum|Fobs|
  !!----  "dis-restr"       Cost(dis-restr)      : Optimization of C2 =Sum{w(dobs-dcal)^2}, w=1/var(d)
  !!----  "Ang-restr"       Cost(Ang-restr)      : Optimization of C3 =Sum{w(Ang_obs-Ang_cal)^2}, w=1/var(Ang)
  !!----  "Tor-restr"       Cost(Tor-restr)      : Optimization of C4 =Sum{w(Tor_obs-Tor_cal)^2}, w=1/var(Tor)
  !!----  "bond-valence"    Cost(bond-valence)   : Optimization of C5 =Sum{|q-BVS|/tot_Atoms}
  !!----  "bvs_coulomb"     C5 + Cost(Coulomb)   : Optimization of C6 =C5+Sum{qi qj/dij}/Nbonds/Sum{qi qj} + Sum{((Ri+Rj)/dij)^18}
  !!----  "FoFc-Powder"     Cost(FoFc-Powder)    : Optimization of C7 =Sum|Gobs-Sum(Fcal)|/Sum|Gobs|
  !!----  "Coordination"    Cost(Coordination)   : Optimization of C8 =Sum|Coord-Efcn|/Sum|Coord|
  !!----  "Anti_Bump"       Cost(Anti_Bump)      : Optimization of C9 =Sum{(dmin/d)**power}
  !!----  "Powder_Profile"  Cost(Powder_Profile) : Optimization of C10=Sum{|yiobs-yicalc|/|yiobs|}
  !!----  "Powder_WProfile" Cost(Powder_WProfile): Optimization of C11=Sum{w{yiobs-yicalc}^2/(N-P)} Equivalent to Chi2
  !!----
  !!----  A series of global variables of this modules are defined for describing crystal
  !!----  structures that are to be optimized against some theoretical or experimental data.
  !!----
  !!----    type (SPG_Type),              public :: SpG
  !!----    type (AtList_Type),           public :: A, A_Clone
  !!----    type (Atoms_Conf_List_Type),  public :: Ac
  !!----    type (Cell_G_Type),           public :: Cell
  !!----    type (RefList_Type),          public :: hkl
  !!----    type (Observation_List_Type), public :: Oh
  !!----
  !!----
  Module GLS_cost_functions
      use CFML_GlobalDeps,                only: Err_CFML,cp
      Use CFML_gSpaceGroups,              only: SPG_Type, Apply_OP, Read_SymTrans_Code
      Use CFML_Geom,                      only: distance,angle_uv,Angle_dihedral, Set_tdist_coordination,&
                                                allocate_coordination_type, coord_info, Set_TDist_Partial_Coordination
      Use CFML_EnBVS,                     only: Cost_BVS, Atoms_Conf_list_Type, species_on_list, &
                                                Allocate_Atoms_Conf_list, &
                                                Cost_BVS_CoulombRep, set_Table_d0_B
      use CFML_strings,                   only: l_case,file_list_type
      Use CFML_Atoms,                     only: AtList_Type

      Use CFML_Metrics,                   only: Cell_G_Type
      Use CFML_Reflections,               only: RefList_Type, SRefl_Type

      Use CFML_Structure_Factors,         only: Structure_Factors,Init_Structure_Factors, &
                                                Calc_StrFactor, Modify_SF
      !Use CFML_Molecular_Crystals,        only: Molecular_Crystal_Type
      Use CFML_Keywords_Code_Parser,      only: NP_Max,NP_Refi, v_Vec,v_Shift,v_Bounds,v_BCon,v_Name, v_List, &
                                                VState_to_AtomsPar, &
                                                Distance_Restraint_Type, Dis_Rest, NP_Rest_Dis, &
                                                Angle_Restraint_Type, Ang_Rest, NP_Rest_Ang,    &
                                                Torsion_Restraint_Type, Tor_Rest, NP_Rest_Tor
      Use GLS_observ

      implicit none
      private

      public::  General_cost_function, Readn_Set_CostFunctPars, Write_CostFunctPars, &
                Write_FinalCost, Write_PRF

      logical,                        public :: err_cost=.false.
      character(len=132),             public :: err_mess_cost=" "
      class(SPG_Type), allocatable,   public :: SpG
      type(AtList_Type),              public :: A, A_Clone
      type(Atoms_Conf_list_Type),     public :: Ac
      class(Cell_G_Type),allocatable, public :: Cell
      type(RefList_Type),             public :: hkl
      type(Observation_List_Type),    public :: Oh
      Integer, parameter,             public :: N_costf=11
      Integer,dimension(0:N_costf),   public :: Icost
      real(kind=cp),   dimension(0:N_costf),   public :: Wcost
      real(kind=cp),   dimension(0:N_costf),   public :: P_cost !Partial cost
      real(kind=cp), public, dimension(:,:),   allocatable  :: trr
      integer,       public, dimension(:,:,:), allocatable  :: Mat


      integer,         public :: Max_Coor
      integer,         public :: irad=2 !Neutrons
      real(kind=cp),   public :: Dmax,wavel
      real(kind=cp)           :: coord_T
      character(len=3),public :: diff_mode="NUC"   ! XRA for x-rays, ELE for electrons

      Type, public :: anti_bump_type
        integer                                     :: nrel
        character(len=2), dimension(:), allocatable :: sp1     !Chemical species 1
        character(len=2), dimension(:), allocatable :: sp2     !Chemical species 2
        real(kind=cp),    dimension(:), allocatable :: damin   !Minimal distances inter-species
        integer,          dimension(:), allocatable :: power   !power of the potential (damin/d)**power
      End Type anti_bump_type

      Type(anti_bump_type), public ::  anti_bump

    Contains

    Subroutine Readn_Set_CostFunctPars(file_dat)
       !---- Arguments ----!
       Type(file_list_type),   intent( in)  :: file_dat
       !---- Local variables ----!
       character(len=132)   :: line
       real(kind=cp)        :: w,tol
       integer              :: i,ier,j,nrel
       logical              :: coordone

       Icost=0
       wcost=0.0
       dmax=3.2
       coordone=.false.
       !First look for antibump relations
       nrel=0
       do j=1,file_dat%nlines
          line=adjustl(l_case(file_dat%line(j)))
          if (line(1:6) =="damin ") nrel=nrel+1
       End do

       if(nrel > 0) then
          anti_bump%nrel=nrel
          allocate(anti_bump%sp1(nrel),anti_bump%sp2(nrel),anti_bump%damin(nrel),anti_bump%power(nrel))
          anti_bump%sp1=" "
          anti_bump%sp2=" "
          anti_bump%damin=0.0
       end if

       i=0
       Do j=1,file_dat%nlines
          line=adjustl(l_case(file_dat%line(j)))
          if (line(1:6) =="damin ") then
              i=i+1
              read(unit=file_dat%line(j)(7:),fmt=*,iostat=ier) anti_bump%sp1(i),anti_bump%sp2(i), &
                                                               anti_bump%damin(i),anti_bump%power(i)
              if(ier /= 0) then
                i=i-1
              end if
          end if
       End do
       if( i < nrel) then
         write(*,*)  " => Warning: some anti-bump relations are badly written! "
         anti_bump%nrel=i
       end if

       do j=1,file_dat%nlines
          line=adjustl(file_dat%line(j))
          line=l_case(line)
          if (len_trim(line) == 0) cycle
          if (line(1:1) =="!" .or. line(1:1) =="#") cycle
          i=index(line,"!")
          if( i /= 0) line=trim(line(1:i-1))

          select case (line(1:4))

              case ("coor")    !Expected coordinations (atoms are already known)
                               !coordination
                 read(unit=line(13:),fmt=*,iostat=ier) (A%Atom(i)%varF(4),i=1,A%natoms)
                 if(ier == 0) then
                   coordone=.true.
                 else
                   A%Atom(:)%varF(4)=0.0
                 end if

              case ("opti")    !Optimization

                 i=index(line,"f2obs-f2cal")
                 if(i == 0) then
                   Icost(0)=0; wcost(0)=0.0
                 else
                   Icost(0)=1
                   read(unit=line(i+11:),fmt=*,iostat=ier) w
                   if(ier /= 0) then
                      wcost(0)=1.0
                   else
                      wcost(0)= w
                   end if
                 end if

                 i=index(line,"fobs-fcal")
                 if(i == 0) then
                   Icost(1)=0; wcost(1)=0.0
                 else
                   Icost(1)=1
                   read(unit=line(i+9:),fmt=*,iostat=ier) w
                   if(ier /= 0) then
                      wcost(1)=1.0
                   else
                      wcost(1)= w
                   end if
                 end if

                 i=index(line,"fofc-powder")
                 if(i == 0) then
                   Icost(7)=0; wcost(7)=0.0
                 else
                   Icost(7)=1
                   read(unit=line(i+11:),fmt=*,iostat=ier) w
                   if(ier /= 0) then
                      wcost(7)=1.0
                   else
                      wcost(7)= w
                   end if
                 end if

                 i=index(line,"dis-restr")
                 if(i == 0) then
                   Icost(2)=0; wcost(2)=0.0
                 else
                   Icost(2)=1
                   read(unit=line(i+9:),fmt=*,iostat=ier) w
                   if(ier /= 0) then
                      wcost(2)=1.0
                   else
                      wcost(2)= w
                   end if
                 end if

                 i=index(line,"ang-restr")
                 if(i == 0) then
                   Icost(3)=0; wcost(3)=0.0
                 else
                   Icost(3)=1
                   read(unit=line(i+9:),fmt=*,iostat=ier) w
                   if(ier /= 0) then
                      wcost(3)=1.0
                   else
                      wcost(3)= w
                   end if
                 end if

                 i=index(line,"tor-restr")
                 if(i == 0) then
                   Icost(4)=0; wcost(4)=0.0
                 else
                   Icost(4)=1
                   read(unit=line(i+9:),fmt=*,iostat=ier) w
                   if(ier /= 0) then
                      wcost(4)=1.0
                   else
                      wcost(4)= w
                   end if
                 end if

                 i=index(line,"bond-valence")
                 if(i == 0) then
                   Icost(5)=0; wcost(5)=0.0
                 else
                   Icost(5)=1
                   read(unit=line(i+12:),fmt=*,iostat=ier) w
                   if(ier /= 0) then
                     wcost(5)=1.0
                   else
                      wcost(5)= w
                   end if
                 end if

                 i=index(line,"bvs_coulomb")
                 if(i == 0) then
                   Icost(6)=0; wcost(6)=0.0
                 else
                   Icost(6)=1
                   read(unit=line(i+11:),fmt=*,iostat=ier) w,tol
                  if(ier /= 0) then
                      wcost(5)=1.0 ; wcost(6)=1.0
                   else
                      wcost(5)= w ; wcost(6)= tol
                   end if
                 end if

                 i=index(line,"coordination")
                 if(i == 0) then
                   Icost(8)=0; wcost(8)=0.0
                 else
                   if(coordone) then
                     Icost(8)=1
                     read(unit=line(i+12:),fmt=*,iostat=ier) w
                     if(ier /= 0) then
                        wcost(8)=1.0
                     else
                        wcost(8)= w
                     end if
                     !Calculate the total coordination from the atom list
                     coord_T=0.0
                     do i=1,A%natoms
                        coord_T= coord_T + A%Atom(i)%varF(4)
                     end do
                     coord_T=100.0/coord_T
                   else
                     Icost(8)=0; wcost(8)=0.0
                   end if
                 end if

                 i=index(line,"anti_bump")
                 if(i == 0) then
                   Icost(9)=0; wcost(9)=0.0
                 else
                   Icost(9)=1
                   read(unit=line(i+9:),fmt=*,iostat=ier) w
                  if(ier /= 0) then
                      wcost(9)=1.0
                   else
                      wcost(9)= w
                   end if
                 end if

                 i=index(line,"powder_profile")
                 if(i == 0) then
                   Icost(10)=0; wcost(10)=0.0
                 else
                   Icost(10)=1
                   read(unit=line(i+14:),fmt=*,iostat=ier) w
                  if(ier /= 0) then
                      wcost(10)=1.0
                   else
                      wcost(10)= w
                   end if
                 end if

                 i=index(line,"powder_wprofile")
                 if(i == 0) then
                   Icost(11)=0; wcost(11)=0.0
                 else
                   Icost(11)=1
                   read(unit=line(i+14:),fmt=*,iostat=ier) w
                  if(ier /= 0) then
                      wcost(11)=1.0
                   else
                      wcost(11)= w
                   end if
                 end if

              case ("dmax")

                 read(unit=line(5:),fmt=*,iostat=ier) w
                 if( ier == 0) dmax=w

              case ("tol ")

                 read(unit=line(5:),fmt=*,iostat=ier) w
                 if( ier == 0) tol=w

              case ("radi")    !Radiation
                 i=index(line,"xra")
                 if(i /= 0)then
                   diff_mode="XRA"
                   irad=1
                 end if
                 i=index(line,"x-ra")
                 if(i /= 0) then
                   diff_mode="XRA"
                   irad=1
                 end if

                 i=index(line,"elect")
                 if(i /= 0) then
                   diff_mode="ELE"
                   irad=3
                 end if

              case ("wave")    !Wavelength
                 read(unit=line(5:),fmt=*,iostat=ier) w
                 if( ier == 0) wavel=w

          end select

       end do

       !Normalize weight vector
        w=sum(Wcost)
        Wcost=Wcost/w

       !Allocate the necesary types and arrays
        bond: if(Icost(5) == 1 .or. Icost(6) == 1 .or. Icost(9) == 1) then !Bond-Valence parameters

          if(A%natoms == 0) then

             err_cost=.true.
             err_mess_cost=" Fatal Error: 0 atoms! => Atom-list must be allocated before calling Readn_Set_CostFunctPars"
             return

          else
             !Construction of the Atom_Conf_List variable "Ac"
             call Allocate_Atoms_Conf_list(A%natoms,Ac)
             Ac%atom(1:A%natoms)=A%atom
             Call Species_on_List(Ac,Spg%Multip,tol)
             if(Err_CFML%Ierr /= 0) then
               err_cost=.true.
               err_mess_cost=trim(Err_CFML%Msg)
               return
             end if

             !Allocation of the global variable "Coord_Info" in Geom_Calculations
             !to be used in distance calculations and Bond-valence
             call Allocate_Coordination_type(A%natoms,Spg%Multip,dmax,Max_coor)

             if(Icost(5) == 1 .or. Icost(6) == 1) then
               call set_Table_d0_B(Ac)
               if(Err_CFML%Ierr /= 0) then
                 err_cost=.true.
                 err_mess_cost=trim(Err_CFML%Msg)
                 return
               end   if
             end if

          end if

        end if  bond

       return
    End Subroutine Readn_Set_CostFunctPars

    Subroutine Write_CostFunctPars(lun)
       !---- Arguments ----!
       integer,   intent( in)    :: lun
       !---- Local variables ----!
       integer              :: i


       Write(unit=lun,fmt="(/,a)")    "=================================="
       Write(unit=lun,fmt="(a)")      "= Cost functions to be minimized ="
       Write(unit=lun,fmt="(a,/)")    "=================================="

       do i=0,n_costf

          if(icost(i) == 0) cycle
          select case (i)

              case (0)    !Optimization "F2obs-F2cal"

                 Write(unit=lun,fmt="(a,f8.4)") &
                 "  => Cost(F2obs-F2cal): Optimization of C0=Sum|F2obs-F2cal|/Sum|F2obs|, with weight: ",wcost(i)

              case (1)    !Optimization "Fobs-Fcal"

                 Write(unit=lun,fmt="(a,f8.4)") &
                 "  => Cost(Fobs-Fcal): Optimization of C1=Sum|Fobs-Fcal|/Sum|Fobs|, with weight: ",wcost(i)

              case (2)     !Optimization "dis-restr"
                 Write(unit=lun,fmt="(a,f8.4)") &
                 "  => Cost(dis-restr): Optimization of C2=Sum{w(dobs-dcal)^2}, w=1/var(d),with weight: ", wcost(i)

              case (3)     !Optimization "Ang-restr"
                 Write(unit=lun,fmt="(a,f8.4)") &
                 "  => Cost(Ang-restr): Optimization of C3=Sum{w(Ang_obs-Ang_cal)^2}, w=1/var(Ang),with weight: ", wcost(i)

              case (4)     !Optimization "Tor-restr"
                 Write(unit=lun,fmt="(a,f8.4)") &
                 "  => Cost(Tor-restr): Optimization of C4=Sum{w(Tor_obs-Tor_cal)^2}, w=1/var(Tor),with weight: ", wcost(i)

              case (5)     !Optimization "bond-valence"
                 Write(unit=lun,fmt="(a,f8.4)") &
                 "  => Cost(bond-valence): Optimization of C5=Sum{|q-BVS|/tot_Atoms}, with weight: ", wcost(i)

              case (6)     !Optimization "bvs_coulomb"
                 Write(unit=lun,fmt="(a,f8.4)") &
                 "  => Cost(bond-valence): Optimization of C5=Sum{|q-BVS|/tot_Atoms}},                  with weight: ", wcost(5)
                 Write(unit=lun,fmt="(a,f8.4)") &
                 "  => Cost(Coulomb): Optimization of C6=Sum{qiqj/dij}/Nbonds/Sum{qiqj}+Sum{((Ri+Rj)/dij)^18}, with weight: ", wcost(6)

              case (7)    !Optimization "FoFc-Powder"
                 Write(unit=lun,fmt="(a,f8.4)") &
                 "  => Cost(FoFc-Powder): Optimization of C7=Sum|Gobs-Sum(Fcal)|/Sum|Gobs|, with weight: ",wcost(i)

              case (8)    !Optimization "Coordination"
                 Write(unit=lun,fmt="(a,f8.4)") &
                 "  => Cost(Coordination): Optimization of C8=Sum|Coord-Efcn|/Sum|Coord|, with weight: ",wcost(i)

              case (9)    !Optimization "Anti_Bump"
                 Write(unit=lun,fmt="(a,f8.4)") &
                 "  => Cost(Anti_Bump): Optimization of C9=Sum{(dmin/d)**power}, with weight: ",wcost(i)

              case (10)    !Optimization "Powder_Profile"
                 Write(unit=lun,fmt="(a,f8.4)") &
                 "  => Cost(Powder_Profile): Optimization of C10=Sum{|yiobs-yicalc|/|yiobs|}, with weight: ",wcost(i)

              case (11)    !Optimization "Powder_WProfile"
                 Write(unit=lun,fmt="(a,f8.4)") &
                 "  => Cost(Powder_WProfile): Optimization of C11=Sum{w(yiobs-yicalc)^2/(N-P)}=Chi2, with weight: ",wcost(i)

          end select

       end do


       return
    End Subroutine Write_CostFunctPars

    Subroutine Write_FinalCost(lun)
       !---- Arguments ----!
       integer,   intent( in)    :: lun
       !---- Local variables ----!
       integer              :: i,j


       Write(unit=lun,fmt="(/,a)")    "====================================="
       Write(unit=lun,fmt="(a)")      "= Final Cost of minimized functions ="
       Write(unit=lun,fmt="(a,/)")    "====================================="

       do i=0,n_costf

          if(icost(i) == 0) cycle

          select case (i)

              case (0)    !Optimization "F2obs-F2cal"

                 Write(unit=lun,fmt="(a,f8.4,a,f12.4)") &
                 "  => Cost(F2obs-F2cal): Optimization of C1=Sum|F2obs-F2cal|/Sum|F2obs|, with weight: ",wcost(i),&
                 "  Final Cost: ",P_cost(0)

              case (1)    !Optimization "Fobs-Fcal"

                 Write(unit=lun,fmt="(a,f8.4,a,f12.4)") &
                 "  => Cost(Fobs-Fcal): Optimization of C1=Sum|Fobs-Fcal|/Sum|Fobs|, with weight: ",wcost(i),&
                 "  Final Cost: ",P_cost(1)

              case (2)     !Optimization "dis-restr"
                 Write(unit=lun,fmt="(a,f8.4,a,f12.4)") &
                 "  => Cost(dis-restr): Optimization of C2=Sum{w(dobs-dcal)^2}, w=1/var(d),with weight: ", wcost(i),&
                 "  Final Cost: ",P_cost(2)


              case (3)     !Optimization "Ang-restr"
                 Write(unit=lun,fmt="(a,f8.4,a,f12.4)") &
                 "  => Cost(Ang-restr): Optimization of C3=Sum{w(Ang_obs-Ang_cal)^2}, w=1/var(Ang),with weight: ", wcost(i),&
                 "  Final Cost: ",P_cost(3)

              case (4)     !Optimization "Tor-restr"
                 Write(unit=lun,fmt="(a,f8.4,a,f12.4)") &
                 "  => Cost(Tor-restr): Optimization of C4=Sum{w(Tor_obs-Tor_cal)^2}, w=1/var(Tor),with weight: ", wcost(i),&
                 "  Final Cost: ",P_cost(4)

              case (5)     !Optimization "bond-valence"
                 Write(unit=lun,fmt="(a,f8.4,a,f12.4)") &
                 "  => Cost(bond-valence): Optimization of C5=Sum{|q-BVS|/tot_Atoms}, with weight: ", wcost(i),&
                 "  Final Cost: ",P_cost(5)

              case (6)     !Optimization "bvs_coulomb"
                 Write(unit=lun,fmt="(a,f8.4,a,f12.4)") &
                 "  => Cost(bond-valence): Optimization of C5=Sum{|q-BVS|/tot_Atoms}}, with weight: ", wcost(5),&
                 "  Final Cost: ",P_cost(5)
                 Write(unit=lun,fmt="(a,f8.4,a,f12.4)") &
                 "  => Cost(Coulomb): Optimization of C6=Sum{qiqj/dij}/Nbonds/Sum{qiqj}+Sum{((Ri+Rj)/dij)^18}, with weight: ", wcost(6),&
                 "  Final Cost: ",P_cost(6)

              case (7)    !Optimization "FoFc-Powder"

                 Write(unit=lun,fmt="(a,f8.4,a,f12.4)") &
                 "  => Cost(FoFc-Powder): Optimization of C7=Sum|Gobs-Sum(Fcal)|/Sum|Gobs|, with weight: ",wcost(i),&
                 "  Final Cost: ",P_cost(7)

              case (8)    !Optimization "Coordination"

                 Write(unit=lun,fmt="(a,f8.4,a,f12.4,/)") &
                 "  => Cost(Coordination): Optimization of C8=Sum|Coord-Efcn|/Sum|Coord|, with weight: ",wcost(i),&
                 "  Final Cost: ",P_cost(8)
                 do j=1,Ac%natoms
                   if(Ac%Atom(j)%varF(4) < 0.001) cycle
                   Write(unit=lun,fmt="(a,2f8.4)")  "  Obs-Calc Coordination for Atom "//Ac%Atom(j)%lab, &
                                                    Ac%Atom(j)%varF(4),Ac%Atom(j)%varF(3)
                 end do

              case (9)    !Optimization "Anti Bump"
                 Write(unit=lun,fmt="(a,f8.4,a,f12.4,/)") &
                 "  => Cost(Anti_Bump): Optimization of C9=Sum{(dmin/d)**power}, with weight: ",wcost(i),&
                 "  Final Cost: ",P_cost(9)


              case (10)    !Optimization "Powder_Profile"
                 Write(unit=lun,fmt="(a,f8.4,a,f12.4,/)") &
                 "  => Cost(Powder_Profile): Optimization of C10=Sum{|yiobs-yicalc|/|yiobs|}, with weight: ",wcost(i),&
                 "  Final Cost: ",P_cost(10)

              case (11)    !Optimization "Powder_WProfile"
                 Write(unit=lun,fmt="(a,f8.4,a,f12.4,/)") &
                 "  => Cost(Powder_WProfile): Optimization of C11=Sum{w(yiobs-yicalc)^2/(N-P)}, with weight: ",wcost(i),&
                 "  Final Cost: ",P_cost(10)


          end select

       end do


       return
    End Subroutine Write_FinalCost

    !!---- Subroutine General_Cost_function(v,cost)
    !!----  real(kind=cp),dimension(:),    intent( in):: v
    !!----  real(kind=cp),                 intent(out):: cost
    !!----
    !!----  This is the subroutine playing the role of cost function passed
    !!----  to the Simulated Annealing procedure (either mono or multi configurational)
    !!----  On input 'v' is the vector state and on output 'cost' is the value of the cost
    !!----  function. At the end of the SAnn procedure a call to this function is done using
    !!----  the best configuration found. and the atoms parameters 'A' are updated to that of
    !!----  the best values by a call to VState_to_AtomsPar.
    !!----
    !!----  Updated: December 2014
    !!----
    Subroutine General_Cost_function(v,cost)
      real(kind=cp),dimension(:),    intent( in):: v
      real(kind=cp),                 intent(out):: cost
      !---- Local variables ----!
      integer               :: i,ic, nlist=1, numv
      integer, dimension(1) :: List
      logical               :: tdist_called



      v_shift(1:NP_Refi)=v(1:NP_Refi)-v_vec(1:NP_Refi)  !Calculate the shifts w.r.t. old configuration
      v_vec(1:NP_Refi)=v(1:NP_Refi)

      numv=count(abs(v_shift) > 0.00001, dim=1)

      tdist_called=.false.
      if(numv == 1) then
         i=maxloc(abs(v_shift),dim=1)
         List(1)=v_list(i)
         call VState_to_AtomsPar(A) !Update Atomic parameters with the proper constraints

         cost=0.0

         do ic=0,N_costf

            if(Icost(ic) == 0) cycle

            Select Case(ic)

               case(0)      !Experimental F2obs-F2cal diffraction pattern
                     call Modify_SF(hkl,A,SpG,List,Nlist,partyp="CO",mode=diff_mode)
                     call Cost_F2obsF2cal(P_cost(0))
                     cost=cost+ P_cost(0)* WCost(0)

               case(1)      !Experimental Fobs-Fcalc diffraction pattern
                     call Modify_SF(hkl,A,SpG,List,Nlist,partyp="CO",mode=diff_mode)
                     call Cost_FobsFcal(P_cost(1))
                     cost=cost+ P_cost(1)* WCost(1)

               case(2)      !Distance restraints
                     call Cost_Dis_Rest_Partial(List(1),P_cost(2))
                     cost=cost+ P_cost(2)* WCost(2)

               case(3)      !Angle restraints
                     call Cost_Ang_Rest_Partial(List(1),P_cost(3))
                     cost=cost+ P_cost(3)* WCost(3)

               case(4)      !Torsion angle restraints
                     call Cost_Tor_Rest_Partial(List(1),P_cost(4))
                     cost=cost+ P_cost(4)* WCost(4)

               case(5)      !Bond-Valence
                     if(.not. tdist_called) then
                       call Set_TDist_Partial_Coordination(List(1),max_coor, Dmax, Cell, SpG, A)
                       tdist_called=.true.
                     end if
                     call Cost_BVS(Ac, P_cost(5))
                     cost=cost+ P_cost(5)* WCost(5)

               case(6)      !Bond-Valence+ Coulomb repulsion
                     if(.not. tdist_called) then
                       call Set_TDist_Partial_Coordination(List(1),max_coor, Dmax, Cell, SpG, A)
                       tdist_called=.true.
                     end if
                     call Cost_BVS_CoulombRep(Ac, P_cost(5),P_cost(6))
                     cost=cost+ P_cost(5)* WCost(5)+P_cost(6)* WCost(6)

               case(7)      !FoFc-Powder
                     call Modify_SF(hkl,A,SpG,List,Nlist,partyp="CO",mode=diff_mode)
                     call Cost_FoFc_powder(P_cost(7))
                     cost=cost+ P_cost(7)* WCost(7)

               case(8)      !Coordination
                     call Cost_Coordination(P_cost(8))
                     cost=cost+ P_cost(8)* WCost(8)

               case(9)      !Anti-Bumb functions
                     if(.not. tdist_called) then
                       call Set_TDist_Partial_Coordination(List(1),max_coor, Dmax, Cell, SpG, A)
                       tdist_called=.true.
                     end if
                     call Cost_dist_min(P_cost(9))
                     cost=cost+ P_cost(9)* WCost(9)

               case(10,11)     !Powder profile obtimization of Rp or Chi2
                     call Modify_SF(hkl,A,SpG,List,Nlist,partyp="CO",mode=diff_mode)
                     call Cost_Powder_Profile(P_cost(ic))
                     cost=cost+ P_cost(ic)* WCost(ic)

            End Select
         end do

      else            !New configuration

         call VState_to_AtomsPar(A,mode="V")   !Update Atomic parameters with the proper constraints
         cost=0.0

         do ic=0,N_costf

            if(Icost(ic) == 0) cycle

            Select Case(ic)

               case(0)      !Experimental F2obs-F2calc diffraction pattern
                     call Structure_Factors(hkl,A,SpG,mode=diff_mode,lambda=wavel)
                     call Cost_F2obsF2cal(P_cost(0))
                     cost=cost+ P_cost(0)* WCost(0)

               case(1)      !Experimental Fobs-Fcalc diffraction pattern
                     call Structure_Factors(hkl,A,SpG,mode=diff_mode,lambda=wavel)
                     call Cost_FobsFcal(P_cost(1))
                     cost=cost+ P_cost(1)* WCost(1)

               case(2)      !Distance restraints
                     call Cost_Dis_Rest(P_cost(2))
                     cost=cost+ P_cost(2)* WCost(2)

               case(3)      !Angle restraints
                     call Cost_Ang_Rest(P_cost(3))
                     cost=cost+ P_cost(3)* WCost(3)

               case(4)      !Torsion angle restraints
                     call Cost_Tor_Rest(P_cost(4))
                     cost=cost+ P_cost(4)* WCost(4)

               case(5)      !Bond-Valence
                     if(.not. tdist_called) then
                       call Set_TDist_Coordination(max_coor, Dmax, Cell, SpG, A)
                       tdist_called=.true.
                     end if
                     call Cost_BVS(Ac, P_cost(5))
                     cost=cost+ P_cost(5)* WCost(5)

               case(6)      !Bond-Valence+ Coulomb repulsion
                     if(.not. tdist_called) then
                       call Set_TDist_Coordination(max_coor, Dmax, Cell, SpG, A)
                       tdist_called=.true.
                     end if
                     call Cost_BVS_CoulombRep(Ac, P_cost(5),P_cost(6))
                     cost=cost+ P_cost(5)* WCost(5)+P_cost(6)* WCost(6)

               case(7)      !FoFc-Powder
                     call Structure_Factors(hkl,A,SpG,mode=diff_mode,lambda=wavel_int)
                     call Cost_FoFc_powder(P_cost(7))
                     cost=cost+ P_cost(7)* WCost(7)

               case(8)      !Coordination
                     call Cost_Coordination(P_cost(8))
                     cost=cost+ P_cost(8)* WCost(8)

               case(9)      !Anti-Bumb functions
                     if(.not. tdist_called) then
                       call Set_TDist_Coordination(max_coor, Dmax, Cell, SpG, A)
                       tdist_called=.true.
                     end if
                     call Cost_dist_min(P_cost(9))
                     cost=cost+ P_cost(9)* WCost(9)

               case(10,11)     !Powder profile obtimization of Rp or Chi2
                     call Structure_Factors(hkl,A,SpG,mode=diff_mode,lambda=wavel)
                     call Cost_Powder_Profile(P_cost(ic))
                     cost=cost+ P_cost(ic)* WCost(ic)

            End Select
         end do
      end if

    End Subroutine General_Cost_function



    Subroutine Cost_F2obsF2cal(cost)
       real(kind=cp),   intent(out):: cost
       !---- Local variables ----!
       integer        :: i,n
       real(kind=cp)  :: delta,sumcal

       n=hkl%Nref
       Select Type (R => hkl%ref)
         Type is (SRefl_Type)
          sumcal=sum(abs(R(1:n)%Fc(irad)**2))
          ScaleFact=1.0
          if(sumcal > 0.0000001) ScaleFact=SumGobs/sumcal
          cost=0.0
          do i=1,hkl%Nref
            delta=R(i)%Fo(irad)-ScaleFact*R(i)%Fc(irad)**2
            cost=cost+abs(delta)
          end do
       End Select
       cost=100.0*cost/SumGobs

    End Subroutine Cost_F2obsF2cal

    Subroutine Cost_FobsFcal(cost)
       real(kind=cp),  intent(out):: cost
       !---- Local variables ----!
       integer        :: i,n
       real(kind=cp)  :: delta,sumcal

       n=hkl%Nref
       Select Type (R => hkl%ref)
         Type is (SRefl_Type)
            sumcal=sum(abs(R(1:n)%Fc(irad)))
            ScaleFact=1.0
            if(sumcal > 0.0000001) ScaleFact=SumGobs/sumcal
            cost=0.0
            do i=1,hkl%Nref
              delta=R(i)%Fo(irad)-ScaleFact*R(i)%Fc(irad)
              cost=cost+abs(delta)
            end do
       End Select
       cost=100.0*cost/SumGobs

    End Subroutine Cost_FobsFcal

    Subroutine Cost_FoFc_Powder(cost)
       real(kind=cp), intent(out):: cost
       !---- Local variables ----!
       integer      :: i,j,n
       real(kind=cp):: delta,sumcal,over

       n=hkl%Nref
       Select Type (R => hkl%ref)
         Type is (SRefl_Type)
           sumcal=sum(abs(R(1:n)%Fc(irad)))
           ScaleFact=1.0
           if(sumcal > 0.0000001) ScaleFact=SumGobs/sumcal
           cost=0.0
           do i=1,Oh%Nobs
             over=0.0
             do j=1,Oh%Ob(i)%ncont
               n=Oh%Ob(i)%p(j)
               over=over+R(n)%Fc(irad)
             end do
             delta=Oh%Ob(i)%Gobs-ScaleFact*over
             cost=cost+abs(delta)
           end do
       End Select
       cost=100.0*cost/SumGobs

    End Subroutine Cost_FoFc_Powder


    Subroutine Cost_Powder_Profile(cost)
       real(kind=cp), intent(out):: cost
       !---- Local variables ----!
       integer       :: i,j,nn
       real(kind=cp) :: delt,fcal,scalef,ww,pw

       fcal=0.0
       Select Type (R => hkl%ref)
         Type is (SRefl_Type)
            do i=1,sprof%npts
              do j=1,sprof%prf(i)%nref
                nn=sprof%prf(i)%ref_ini+j-1
                !ik=sprof%prf(i)%nref-j+1
                fcal=fcal+ScaleFact*sprof%prf(i)%omegap(j) * R(nn)%Fc(irad) * R(nn)%Fc(irad)
              end do
            end do
       End Select
       Scalef=sprof%sumap/fcal
       ScaleFact=Scalef*ScaleFact

       !---------------------------------------------------------------------
       ! Second Loop over profile points to calculate the cost function
       !----------------------------------------------------------------------
       Select Type (R => hkl%ref)
         Type is (SRefl_Type)
           if(Weight_Sim) then
             cost=0.0
             do i=1,sprof%npts
               fcal=0.0
               do j=1,sprof%prf(i)%nref
                 nn=sprof%prf(i)%ref_ini+j-1
                 fcal=fcal+ScaleFact*sprof%prf(i)%omegap(j)* R(nn)%Fc(irad) * R(nn)%Fc(irad)
               end do
               sprof%prf(i)%ycal=fcal
               delt=sprof%prf(i)%yobs -sprof%prf(i)%ycal

               Select Case(iwgt)
                 Case(1)
                   ww=sprof%meanvar
                 Case(2)
                   pw=(sprof%prf(i)%yobs+2.0*sprof%prf(i)%ycal)/3.0
                   ww=sprof%prf(i)%var + 0.04*pw*pw
                 Case Default
                   ww=sprof%prf(i)%var
               End Select
               cost=cost + delt*delt/ww
             end do
             cost=cost/real(sprof%npts-NP_Refi)    !Chi-squared

           else

             cost=0.0
             do i=1,sprof%npts
               fcal=0.0
               do j=1,sprof%prf(i)%nref
                 nn=sprof%prf(i)%ref_ini+j-1
                 fcal=fcal+ScaleFact*sprof%prf(i)%omegap(j)* R(nn)%Fc(irad) * R(nn)%Fc(irad)
               end do
               sprof%prf(i)%ycal=fcal
               delt=sprof%prf(i)%yobs -sprof%prf(i)%ycal
               cost=cost+ abs(delt)
             end do
             cost=100.0*cost/sprof%sumap

           end if
       End Select
       !if(present(plot)) then
       !   npts(n_pat)=sprof%npts
       !   y(1:sprof%npts,n_pat)=sprof%prf(1:sprof%npts)%yobs
       !   yc(1:sprof%npts,n_pat)=sprof%prf(1:sprof%npts)%ycal
       !   txv(1:sprof%npts,n_pat)=sprof%prf(1:sprof%npts)%scv
       !end if
    End Subroutine Cost_Powder_Profile


    Subroutine Cost_Dis_Rest_Partial(List,cost)
       integer,       intent(in) :: List
       real(kind=cp), intent(out):: cost
       !---- Local variables ----!
       integer                     :: i, nop, i1,i2
       real(kind=cp)               :: w, delta
       real(kind=cp), dimension(3) :: x1,x2,tr

       cost=0.0
       do i=1,NP_Rest_Dis
          i1=Dis_rest(i)%p(1)
          i2=Dis_rest(i)%p(2)
          if(list == i1 .or. list == i2) then
              x1=A%Atom(i1)%x
              x2=A%Atom(i2)%x
              call Read_SymTrans_Code(dis_rest(i)%stcode,nop,tr)
              !x2=Apply_OP(SpG%OP(nop),x2)+tr
              x2=matmul(Mat(:,:,nop),x2)+ trr(:,nop) + tr
              Dis_rest(i)%dcalc=distance(x1,x2,cell)
          end if
          delta=Dis_rest(i)%dobs-Dis_rest(i)%dcalc
          w= 1.0/(Dis_rest(i)%sigma*Dis_rest(i)%sigma)
          cost= cost+delta*delta*w
       end do

    End Subroutine Cost_Dis_Rest_Partial

    Subroutine Cost_Coordination(cost)
       real(kind=cp), intent(out):: cost
       !---- Local variables ----!
       integer          :: i
       real(kind=cp)    :: delta

       cost=0.0
       do i=1,Ac%natoms
          if(Ac%Atom(i)%varF(4) < 0.001) cycle
          delta=abs(Ac%Atom(i)%varF(4)-Ac%Atom(i)%varF(3))
          cost= cost+delta
       end do
       cost=cost*coord_T
       return
    End Subroutine Cost_Coordination

    Subroutine Cost_Dist_Min(cost)
       real(kind=cp),      intent(out):: cost
       !---- Local variables ----!
       integer            :: i,j,k, icm
       real(kind=cp)      :: d
       character(len=2)   :: ch1,ch2

       cost=0.0
       do i=1,Ac%natoms
          icm=coord_info%coord_num(i)
          ch1=Ac%Atom(i)%ChemSymb
          do j=1,icm
             ch2=A%Atom(coord_info%n_cooatm(j,i))%ChemSymb
             do k=1,anti_bump%nrel
                if( ( ch1 == anti_bump%sp1(k) .or. ch1 == anti_bump%sp2(k)) .and. &
                    ( ch2 == anti_bump%sp1(k) .or. ch2 == anti_bump%sp2(k)) ) then
                     d = coord_info%dist(j,i)
                  cost = cost + (anti_bump%damin(k)/d)**anti_bump%power(k)
                end if
             end do
          end do
       end do

    End Subroutine Cost_Dist_Min

    Subroutine Cost_Dis_Rest(cost)
       real(kind=cp),      intent(out):: cost
       !---- Local variables ----!
       integer                     :: i, nop, i1,i2
       real(kind=cp)               :: w, delta
       real(kind=cp), dimension(3) :: x1,x2,tr

       cost=0.0
       do i=1,NP_Rest_Dis
          i1=Dis_rest(i)%p(1)
          i2=Dis_rest(i)%p(2)
          x1=A%Atom(i1)%x
          x2=A%Atom(i2)%x
          call Read_SymTrans_Code(dis_rest(i)%stcode,nop,tr)
          !x2=Apply_OP(SpG%OP(nop),x2)+tr
          x2=matmul(Mat(:,:,nop),x2)+ trr(:,nop) + tr
          Dis_rest(i)%dcalc=distance(x1,x2,cell)
          delta=(Dis_rest(i)%dobs-Dis_rest(i)%dcalc)/Dis_rest(i)%dobs
          w= 1.0/(Dis_rest(i)%sigma*Dis_rest(i)%sigma)
          cost= cost+delta*delta*w
       end do

    End Subroutine Cost_Dis_Rest

    Subroutine Cost_Ang_Rest_Partial(List,cost)
       integer,       intent(in) :: List
       real(kind=cp), intent(out):: cost
       !---- Local variables ----!
       integer                     :: i, nop, i1,i2,i3
       real(kind=cp)               :: w, delta
       real(kind=cp), dimension(3) :: x1,x2,x3,tr

       cost=0.0

       do i=1,NP_Rest_Ang
          i1=Ang_rest(i)%p(1)
          i2=Ang_rest(i)%p(2)
          i3=Ang_rest(i)%p(3)
          if(list == i1 .or. list == i2 .or. list == i3 ) then
              x1=A%Atom(i1)%x
              x2=A%Atom(i2)%x
              call Read_SymTrans_Code(ang_rest(i)%stcode(1),nop,tr)
              !x2=Apply_OP(SpG%OP(nop),x2)+tr
              x2=matmul(Mat(:,:,nop),x2)+ trr(:,nop) + tr
              x3=A%Atom(i3)%x
              call Read_SymTrans_Code(ang_rest(i)%stcode(2),nop,tr)
              !x3=Apply_OP(SpG%OP(nop),x3)+tr
              x3=matmul(Mat(:,:,nop),x3)+ trr(:,nop) + tr
              Ang_rest(i)%Acalc=Angle_UV(x1-x2,x3-x2,cell%GD)
          end if
          delta=Ang_rest(i)%Aobs-Ang_rest(i)%Acalc
          w= 1.0/(Ang_rest(i)%sigma*Ang_rest(i)%sigma)
          cost= cost+delta*delta*w
       end do

    End Subroutine Cost_Ang_Rest_Partial

    Subroutine Cost_Ang_Rest(cost)
       real(kind=cp),      intent(out):: cost
       !---- Local variables ----!
       integer                     :: i, nop, i1,i2,i3
       real(kind=cp)               :: w, delta
       real(kind=cp), dimension(3) :: x1,x2,x3,tr

       cost=0.0

       do i=1,NP_Rest_Ang
          i1=Ang_rest(i)%p(1)
          i2=Ang_rest(i)%p(2)
          i3=Ang_rest(i)%p(3)
          x1=A%Atom(i1)%x
          x2=A%Atom(i2)%x
          call Read_SymTrans_Code(ang_rest(i)%stcode(1),nop,tr)
          !x2=Apply_OP(SpG%OP(nop),x2)+tr
          x2=matmul(Mat(:,:,nop),x2)+ trr(:,nop) + tr
          x3=A%Atom(i3)%x
          call Read_SymTrans_Code(ang_rest(i)%stcode(2),nop,tr)
          !x3=Apply_OP(SpG%OP(nop),x3)+tr
          x3=matmul(Mat(:,:,nop),x3)+ trr(:,nop) + tr
          Ang_rest(i)%Acalc=Angle_UV(x1-x2,x3-x2,cell%GD)
          delta=(Ang_rest(i)%Aobs-Ang_rest(i)%Acalc)/Ang_rest(i)%Aobs
          w= 1.0/(Ang_rest(i)%sigma*Ang_rest(i)%sigma)
          cost= cost+delta*delta*w
       end do

    End Subroutine Cost_Ang_Rest

    Subroutine Cost_Tor_Rest_Partial(List,cost)
       integer,       intent(in) :: List
       real(kind=cp), intent(out):: cost
       !---- Local variables ----!
       integer                     :: i, nop, i1,i2,i3,i4
       real(kind=cp)               :: w, delta
       real(kind=cp), dimension(3) :: x1,x2,x3,x4,tr

       cost=0.0

       do i=1,NP_Rest_tor
          i1=Tor_rest(i)%p(1)
          i2=Tor_rest(i)%p(2)
          i3=Tor_rest(i)%p(3)
          i4=Tor_rest(i)%p(4)
          if(list == i1 .or. list == i2 .or. list == i3 .or. list == i4 ) then
              x1=A%Atom(i1)%x
              x2=A%Atom(i2)%x
              call Read_SymTrans_Code(tor_rest(i)%stcode(1),nop,tr)
              !x2=Apply_OP(SpG%OP(nop),x2)+tr
              x2=matmul(Mat(:,:,nop),x2)+ trr(:,nop) + tr

              x3=A%Atom(i3)%x
              call Read_SymTrans_Code(tor_rest(i)%stcode(2),nop,tr)
              !x3=Apply_OP(SpG%OP(nop),x3)+tr
              x3=matmul(Mat(:,:,nop),x3)+ trr(:,nop) + tr

              x4=A%Atom(i3)%x
              call Read_SymTrans_Code(tor_rest(i)%stcode(3),nop,tr)
              !x4=Apply_OP(SpG%OP(nop),x4)+tr
              x4=matmul(Mat(:,:,nop),x4)+ trr(:,nop) + tr
              x1=matmul(Cell%Cr_Orth_cel,x1)
              x2=matmul(Cell%Cr_Orth_cel,x2)
              x3=matmul(Cell%Cr_Orth_cel,x3)
              x4=matmul(Cell%Cr_Orth_cel,x4)

              tor_rest(i)%Tcalc=Angle_Dihedral(x1,x2,x3,x4)
          end if
          delta=tor_rest(i)%Tobs-tor_rest(i)%Tcalc
          w= 1.0/(Tor_rest(i)%sigma*Tor_rest(i)%sigma)
          cost= cost+delta*delta*w
       end do

    End Subroutine Cost_Tor_Rest_Partial

    Subroutine Cost_Tor_Rest(cost)
       real(kind=cp), intent(out):: cost
       !---- Local variables ----!
       integer                     :: i, nop, i1,i2,i3,i4
       real(kind=cp)               :: w, delta
       real(kind=cp), dimension(3) :: x1,x2,x3,x4,tr

       cost=0.0

       do i=1,NP_Rest_tor
          i1=Tor_rest(i)%p(1)
          i2=Tor_rest(i)%p(2)
          i3=Tor_rest(i)%p(3)
          i4=Tor_rest(i)%p(4)
          x1=A%Atom(i1)%x
          x2=A%Atom(i2)%x
          call Read_SymTrans_Code(tor_rest(i)%stcode(1),nop,tr)
          !x2=Apply_OP(SpG%OP(nop),x2)+tr
          x2=matmul(Mat(:,:,nop),x2)+ trr(:,nop) + tr
          x3=A%Atom(i3)%x
          call Read_SymTrans_Code(tor_rest(i)%stcode(2),nop,tr)
          !x3=Apply_OP(SpG%OP(nop),x3)+tr
          x3=matmul(Mat(:,:,nop),x3)+ trr(:,nop) + tr
          x4=A%Atom(i3)%x
          call Read_SymTrans_Code(tor_rest(i)%stcode(3),nop,tr)
          x4=Apply_OP(SpG%OP(nop),x4)+tr

          x1=matmul(Cell%Cr_Orth_cel,x1)
          x2=matmul(Cell%Cr_Orth_cel,x2)
          x3=matmul(Cell%Cr_Orth_cel,x3)
          x4=matmul(Cell%Cr_Orth_cel,x4)

          tor_rest(i)%Tcalc=Angle_Dihedral(x1,x2,x3,x4)
          delta=tor_rest(i)%Tobs-tor_rest(i)%Tcalc
          w= 1.0/(Tor_rest(i)%sigma*Tor_rest(i)%sigma)
          cost= cost+delta*delta*w
       end do

    End Subroutine Cost_Tor_Rest

    Subroutine Write_Prf(filcod)
       character(len=*), intent(in) :: filcod
       !-----------------------------------------------
       !   L o c a l   V a r i a b l e s
       !-----------------------------------------------
       integer           :: i, iposr, ihkl, irc,nphase,ideltr, i_prf
       real(kind=cp)     :: twtet, dd, scl,yymi,yyma,t1,t2
       character (len=1) :: tb
       character (len=50)  :: forma1
       !-----------------------------------------------
       !check for very high values of intensities and rescal everything in such a case
       ! scl: scale factor scl=1.0 (normal ymax < 1e6, 0.1 multiplier)
       open(newunit=i_prf,file=trim(filcod)//".prf",status="replace",action="write")
       t1=maxval(sprof%prf(:)%yobs)
       t2=maxval(sprof%prf(:)%ycal)
       yyma=max (t1,t2)
       yymi=minval(sprof%prf(:)%yobs)
       scl=1.0
       nphase=1

       do
         if(yyma < 1.0e6) exit !on exit we have the appropriate value of scl
         scl=scl*0.1
         yyma=yyma*scl
       end do
       yymi=yymi*scl
       tb=CHAR(9)

       if(yyma < 100.0) then
        forma1='(f12.4,4(a,f8.4))'
       else if(yyma < 1000.0) then
        forma1='(f12.4,4(a,f8.3))'
       else if(yyma < 10000.0) then
        forma1='(f12.4,4(a,f8.2))'
       else if(yyma < 100000.0) then
        forma1='(f12.4,4(a,f8.1))'
       else
        forma1='(f12.4,4(a,f8.0))'
       end if
       write(i_prf,'(A)') " Observed and calculated profiles at the end of GLOpS"

       write(i_prf,'(i3,i7,5f12.5,i5)') nphase,sprof%npts,wavel_int,wavel_int,0.0,0.0,0.0,0

       write(i_prf,'(3i6)')hkl%nref, 0 , 0

       write(i_prf,'(15a)')' 2Theta',tb,'Yobs',tb,'Ycal',tb,  &
             'Yobs-Ycal',tb,'Backg',tb,'Posr',tb,'(hkl)',tb,'K'

       do  i=1,sprof%npts
         twtet=sprof%prf(i)%scv
         dd=(sprof%prf(i)%yobs-sprof%prf(i)%ycal)*scl
         write(i_prf,forma1) twtet,tb,sprof%prf(i)%yobs*scl,tb,sprof%prf(i)%ycal*scl,tb,  &
             dd-yyma/4.0,tb,-yymi/4.0
       end do

       !Reflections
       iposr=0
       irc=1
       ihkl=0
       ideltr=int(yyma/16)
       do i=1,hkl%nref
         write(i_prf,'(f12.4,9a,i8,a,3i3,a,2i3)')  &
             ttheta(i),tb,'        ',tb,'        ',tb,'        ',  &
             tb,'        ',tb,iposr, tb//'(',hkl%Ref(i)%h,')'//tb,ihkl,irc
       end do
       close(unit=i_prf)

    End Subroutine Write_Prf


  End Module GLS_cost_functions
