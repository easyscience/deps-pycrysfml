 Submodule (CFML_EnBVS) EnBVS_Energy
  implicit none
   contains
    !!----
    !!---- Module Subroutine Ewald(Lattvec,Vol,Ac,e)
    !!----
    !!----   Real(kind=cp), Dimension(3,3),  Intent(in)   :: Lattvec !Transformation matrix from Crystallographic to Cartesian components
    !!----   Real(kind=cp),                  Intent(in)   :: Vol     !Volume of the unit cell
    !!----   Type (Atm_Cell_Type),           Intent(in)   :: Ac      !List of atoms in the unit cell
    !!----   Real(kind=dp),                  Intent(out)  :: e       !Electrostatic lattice energy
    !!----
    !!----   This subroutine computes the electrostatic energy using the Ewald method.
    !!----
    !!----  Update: February - 2018
    !!
    Module Subroutine Ewald(Lattvec,Vol,Ac,e)
      Real(kind=cp), Dimension(3,3),  Intent(in)   :: Lattvec
      Real(kind=cp),                  Intent(in)   :: Vol
      Type (Atm_Cell_Type),           Intent(in)   :: Ac
      Real(kind=8),                   Intent(out)  :: e

      Integer                                         i, j, k, h, l, ik, nk
      Integer, Dimension(3)                        :: ncell, nrcell

      Real(kind=dp), Parameter                      :: k_coul = 14.39963977_dp     ! eV * angstrom
      Real(kind=dp), Parameter                      :: k_boltz = 8.617332358e-5_dp ! eV / K
      Real(kind=dp), Parameter                      :: beta = 0.4_dp
      Real(kind=dp), Parameter                      :: rcut = 12.0_dp              ! angstrom
      Real(kind=dp), Parameter                      :: kcut =  4.0_dp              ! angstrom ^ -1
      Real(kind=dp)                                    rcut2, kcut2, four_beta2, fourpi
      Real(kind=dp)                                 :: r2, e_s, e_l, e_self, fase, sk2
      Real(kind=dp),  Dimension(2)                  :: sk
      Real(kind=dp),  Dimension(3)                  :: latt, rlatt, rdmin, kdmin, r0, r, a, b, c
      Real(kind=cp), Dimension(3,3)                :: rlattvec
      Real(kind=cp), Dimension(:), Allocatable     :: expok
      Real(kind=cp), Dimension(:,:), Allocatable   :: xyz, errorfc, g

      ! Constants

      rcut2         = rcut ** 2
      kcut2         = kcut ** 2
      fourpi      = 4.0_dp * pi
      four_beta2  = 4.0_dp * beta*beta

      ! Compute reciprocal lattice vectors rlattvec

      rlattvec(1:3,1) = cross_product(lattvec(1:3,2),lattvec(1:3,3))
      rlattvec(1:3,2) = cross_product(lattvec(1:3,3),lattvec(1:3,1))
      rlattvec(1:3,3) = cross_product(lattvec(1:3,1),lattvec(1:3,2))

      rlattvec = rlattvec * tpi / Vol

      ! Compute module of lattice and reciprocal lattice vectors

      Do i = 1 , 3
         latt(i)  = Dot_product(lattvec(1:3,i),lattvec(1:3,i))
         rlatt(i) = Dot_product(rlattvec(1:3,i),rlattvec(1:3,i))
         latt(i)  = Sqrt(latt(i))
         rlatt(i) = Sqrt(rlatt(i))
      End Do

      Do i = 1 , 3
         rdmin(i)  = Dot_product(lattvec(1:3,i),rlattvec(1:3,i))
         kdmin(i)  = rdmin(i) / latt(i)
         rdmin(i)  = rdmin(i) / rlatt(i)
         ncell(i)  = Int(rcut / rdmin(i)) + 1
         nrcell(i) = Int(kcut / kdmin(i)) + 1
      End Do

      ! Direct to Cartesian Coordinates

      Allocate(xyz(3,Ac%Nat))
      xyz=0.0
      Do i = 1 , Ac%Nat
        xyz(:,i)= matmul(lattvec,Ac%XYZ(:,i))
      End Do

      ! - Arrays construction: ERRORFC, G, EXPOK

      ! - ERROFC

      Allocate(errorfc(Ac%Nat,Ac%Nat))

      Do i = 1 , Ac%Nat
         Do j = 1 , Ac%Nat
            errorfc(i,j) = 0.
            r0 = xyz(:,j) - xyz(:,i)
            Do h = -ncell(1) , ncell(1)
               Do k = -ncell(2) , ncell(2)
                  Do l = -ncell(3) , ncell(3)
                     If (i /= j .Or. &
                         h /= 0 .Or. &
                         k /= 0 .Or. &
                         l /= 0) Then
                         r = r0 + h * lattvec(:,1) + k * lattvec(:,2) + l * lattvec(:,3)
                        r2 = Dot_Product(r,r)
                        If (r2 <= rcut2) Then
                           r(1)         = Sqrt(r2)
                           errorfc(i,j) = errorfc(i,j) + erfc(beta * r(1)) / r(1)
                        End If

                     End If
                  End Do
               End Do
            End Do
         End Do
      End Do

      ! - G:  reciprocal vectors used in the sum

      nk = (2 * nrcell(1) + 1) * (2 * nrcell(2) + 1) * (2 * nrcell(3) + 1)

      Allocate(g(1:nk,1:4))

      nk = 0
      i  = 0
      g  = 0

      Do h = - nrcell(1) , nrcell(1)
         a = h * rlattvec(:,1)
         Do k = - nrcell(2) , nrcell(2)
            b = k * rlattvec(:,2)
            Do l = - nrcell(3) , nrcell(3)
               c = l * rlattvec(:,3)
               r = a  + b + c
               r2 = Dot_Product(r,r)
               If (r2 <= kcut2 .And. r2 > 0.0d0) Then
                  nk        = nk + 1
                  g(nk,1:3) = r
                  g(nk,4)   = r2
               End If
            End Do
         End Do
      End Do

      ! - EXPOK

      Allocate(expok(1:nk))

      Do ik = 1 , nk
         expok(ik) = Exp((-g(ik,4) / four_beta2)) / g(ik,4)
      end Do

      ! - Compute energy

      e_self = 0.0_dp
      e_s    = 0.0_dp
      e_l    = 0.0_dp

      ! - E_SELF

      Do i = 1 , Ac%Nat
         e_self = e_self + Ac%Charge(i) * Ac%Charge(i)
      End Do

      ! - E_S (Real space contribution)

      Do i = 1 , Ac%Nat
         Do j = 1 , Ac%Nat
            e_s = e_s + Ac%Charge(i) * Ac%Charge(j) * errorfc(i,j)
         End Do
      End Do

      ! E_L (Reciprocal space contribution)

      Do ik = 1 , nk
         sk(1:2) = 0.0d0
         Do j = 1 , Ac%Nat
            fase = Dot_product(xyz(1:3,j),g(ik,1:3))
            sk(1) = sk(1) + Ac%Charge(j) * cos(fase)
            sk(2) = sk(2) + Ac%Charge(j) * Sin(fase)
         End Do
         sk2 = sk(1) ** 2 + sk(2) ** 2
         e_l = e_l + expok(ik) * sk2
      End Do
      e_s    = e_s * k_coul / 2.0
      e_l    = e_l * k_coul * fourpi / 2.0 / Vol
      e_self = e_self * k_coul * beta  / Sqrt(pi)
      e      = (e_s + e_l - e_self) / Ac%Nat
    End Subroutine Ewald



    !!----
    !!---- Module Subroutine Set_Charges(SpGr,Cell,A,filcod)
    !!----     class (SpG_Type),      Intent(in)    :: SpGr
    !!----     Type (Cell_G_Type),          Intent(in)    :: Cell
    !!----     Type (AtList_Type),          Intent(inout) :: A
    !!----     Real(kind=cp), Dimension(*), Intent(out)   :: charges
    !!----     Character(len=*), Optional,  Intent(in)    :: filcod
    !!----
    !!----    This subroutine sets atomic charges for object A using
    !!----    bond-valence sums. Output is stored in Formal_Charges.fch
    !!----    or filcod.fch if filcod is provided.
    !!----
    !!---- Update: July - 2016
    !!
    Module Subroutine Set_Formal_Charges(SpGr,Cell,A,eps_val,iwrt)
      !---- Arguments ----!
      class (SpG_Type),Intent(in)    :: SpGr
      Type (Cell_G_Type),    Intent(in)    :: Cell
      Type (AtList_Type),    Intent(in out):: A
      Real(kind=cp),Optional,Intent(in)    :: eps_val
      Integer,      Optional,Intent(in)    :: iwrt
      !---- Local variables ----!
      Type (Atm_Cell_Type)                       :: Ac,Bc
      Type (Point_Orbit)                         :: orbit

      Character(len=2)                           :: label

      Integer                                    :: i, j, k, l, n, m, icomb, p, diff, penalty_aux,lun
      Integer                                    :: nelements, nanions_candidate, ncombinations, nsolutions, nunusual_aux
      Integer                                    :: nsol_usual, nelemmix, nequiv_sol, npenalty_min, penalty_min
      Integer, Parameter                         :: massive_limit = 100
      Integer, Dimension(:), Allocatable         :: anion_candidate, first_neighbor, iontype, nvalences, contador, nunusual
      Integer, Dimension(:), Allocatable         :: atom2elem, elemmix
      Integer, Dimension(:), Allocatable         :: penalty, Ac2Bc
      Integer, Dimension(:,:), Allocatable       :: valence

      Real(kind=cp)                              :: rmin, total_charge, energy_aux, norm, epsv, old_eps
      Real(kind=cp), Parameter                   :: ZEROCHARGE = 1.0e-4
      Real(kind=cp)                              :: Dmax=4.0, Dangle=0.0
      Real(kind=dp),  Dimension(:), Allocatable  :: energy
      Real(kind=cp), Dimension(:), Allocatable   :: sumocc, solution_aux
      Real(kind=cp), Dimension(:,:), Allocatable :: solution, combination, solution_new !, xyz

      Logical                                    :: error_pairs=.False.,error_element=.False.
      Logical                                    :: warning_valence
      Logical                                    :: newelement, incremento, usual, ordered, xyz_stored, newsolution, identical

      !--- Local Type ---!
      Type Element
         Character(len=2)                        :: ChemSymb
         Character(len=10)                       :: Lab
         Integer                                 :: Z
         Integer                                 :: Typel
      End Type Element

      Type(Element), Dimension(:), Allocatable   :: Elem

      call clear_error()
      epsv=0.01
      lun=6
      if(present(iwrt)) lun=iwrt
      if(present(eps_val)) epsv=eps_val

      Write(unit=lun,fmt="(/,/,7(a,/))")                           &
           "             ============================"           , &
           "             ====== FORMAL CHARGES ======"           , &
           "             ============================"           , &
           "    ***********************************************" , &
           "    *  Formal charges from  *.cfl or *.cif files  *" , &
           "    ***********************************************" , &
           "      (Nebil A. Katcho - ILL, version: July 2018)"
      Write(unit=lun,fmt="(a,/)") " "

      ! -------------------
      ! Build the unit cell
      ! -------------------
      call Allocate_Atoms_Cell(0,0,0.0,Ac) !Deallocate Ac
      Call Allocate_Atoms_Cell(A%Natoms,SpGr%Multip,0.0,Ac) !Allocate Ac
      old_eps=epss
      Call Set_Eps_Math(epsv) !needed for well controlling the calculation of multiplicities
      Write(unit=lun,fmt="(a,i3)")  "    Space Group Number: ", SpGr%NumSpg
      Write(unit=lun,fmt="(a,a,/)") "    Space Group Symbol: ", SpGr%Spg_Symb

      Ac%Nat = 0
      j      = 1

      Do i = 1 , A%Natoms
         Ac%Nat = Ac%Nat + A%Atom(i)%Mult
         Ac%Lab(j:Ac%Nat) = A%Atom(i)%Lab
         Call Get_Orbit(A%Atom(i)%X,SpGr,orbit)
         If (orbit%mult /= A%Atom(i)%Mult) Then
            Err_CFML%Ierr=1
            Err_CFML%Msg = "Error in multiplicities. Check space group (or increase epsg)"
            Return
         End If
         Ac%XYZ(:,j:Ac%Nat)=orbit%pos(:,:)
         j = j + A%Atom(i)%Mult
      End Do

      ! We build Bc. It is build from Ac, we only avoid to repeat lattice sites. Bc and Ac
      ! are different when two different atomic species occupy the same lattice sites. Bc
      ! is needed to compute the electrostatic energy.
      Allocate(Bc%XYZ(3,Ac%Nat),Bc%Charge(Ac%Nat))
      Allocate(Ac2Bc(Ac%Nat)) ! maps atoms of Ac into Bc
      Allocate(sumocc(Ac%Nat))

      sumocc(:) = 0.
      k         = 0
      Do i = 1 , A%Natoms
         Do m = 1 , A%Atom(i)%Mult
            xyz_stored = .False.
            j = 1
            k = k + 1
            Do   !While (.Not. xyz_stored .And. j <= Bc%Nat)
               If (xyz_stored .Or. j > Bc%Nat) Exit
               If (Abs(Bc%XYZ(1,j)-Ac%XYZ(1,k)) < 0.01 .And. &
                   Abs(Bc%XYZ(2,j)-Ac%XYZ(2,k)) < 0.01 .And. &
                   Abs(Bc%XYZ(3,j)-Ac%XYZ(3,k)) < 0.01) Then
                  xyz_stored = .True.
               Else
                  j = j + 1
               End If
            End Do
            If (.Not. xyz_stored) Then
               Bc%Nat           = Bc%Nat + 1
               Bc%XYZ(:,Bc%Nat) = Ac%XYZ(:,k)
            End If
            Ac2Bc(k)       = Bc%Nat
            sumocc(Bc%Nat) = sumocc(Bc%Nat) + A%Atom(i)%Occ
         End Do

      End Do

      ! We compute the number of elements and the number of elements that can be considered
      ! as anions

      Call Set_Chem_Info()
      Call Set_Oxidation_States_Table()
      Call Set_Common_Oxidation_States_Table()

      nelements         = 0
      nanions_candidate = 0

      Allocate(anion_candidate(A%natoms),atom2elem(A%natoms))

      Do i = 1 , A%natoms
         newelement         = .True.
         anion_candidate(i) = 0
         Label = A%Atom(i)%ChemSymb
         A%Atom(i)%ChemSymb = Get_Chem_Symb(label)
         A%Atom(i)%Z        = Get_Z_Symb(A%Atom(i)%ChemSymb)
         Do j = 1 , i - 1
            If (A%Atom(i)%ChemSymb == A%Atom(j)%ChemSymb) Then
               newelement = .False.
               atom2elem(i) = atom2elem(j)
            End If
         End Do

         j = 1
         Do
            If (j > 11) Exit
            If (anion_candidate(i) /= 0) Exit
            If (OxStates_Table(j,A%Atom(i)%Z) < 0) Then
               anion_candidate(i) = -1
               nanions_candidate  = nanions_candidate + 1
            End If
            j = j + 1
         End Do

         If (anion_candidate(i) == -1) nanions_candidate = nanions_candidate + 1
         If (newelement) Then
            nelements    = nelements + 1
            atom2elem(i) = nelements
         End If
      End Do

      Write(unit=lun,fmt="(a,i2/)") " => Number of elements found: ", nelements
      Write(unit=lun,fmt="(a,/)") " => Searching ions that can be anions...."
      Write(unit=lun,fmt="(tr4,a)") "----------------"
      Write(unit=lun,fmt="(tr4,a)") "Anion candidates"
      Write(unit=lun,fmt="(tr4,a)") "----------------"

      Do i = 1 , A%natoms
         If (anion_candidate(i) == 0) Then
            Write(unit=lun,fmt="(tr4,a4,tr9,a)") A%Atom(i)%Lab, " NO"
         Else
            Write(unit=lun,fmt="(tr4,a4,tr9,a)") A%Atom(i)%Lab, "YES"
         End If
      End Do

      Write(unit=lun,fmt="(tr4,a,/)") "----------------"

      If (nelements == 1) Then
         ! We have an element, not a compound
         ! Formal charges = 0.
         Do i = 1 , A%nAtoms
            A%Atom(i)%Charge = 0
         End Do
         Write(unit=lun,fmt="(/,a)") " => This is an element. All charges are have been set to zero"

      Else If (nanions_candidate == 0) Then
         ! No anions
         ! Formal charges = 0.0
         Do i = 1 , A%nAtoms
            A%Atom(i)%Charge = 0
         End Do
         Write(unit=lun,fmt="(/,a)") " => No anions candidates have been found"
         Write(unit=lun,fmt="(tr4,a)") "All charges have been set to zero"

      Else
         ! Here we determine which ion is a cation, which is an anion
         !     1. we determine the first neighbor of each atom
         !     2. each pair must be a cation-anion pair

         Call Calc_Dist_Angle(Dmax,Dangle,Cell,SpGr,A)
         Allocate(first_neighbor(A%natoms))
         first_neighbor = 0

         ! First neighbors

         Write(unit=lun,fmt="(a,/)") " => Computing first neighbor distance, needed to define cations and anions..."
         Write(unit=lun,fmt="(tr4,a)") "-----------------------"
         Write(unit=lun,fmt="(tr4,a)")  "First neighbor distance"
         Write(unit=lun,fmt="(tr4,a)")  "-----------------------"

         Do i = 1 , A%natoms
            rmin = 1.0e8_cp
            Do j = 1 , coord_info%Max_coor
               If (coord_info%N_CooAtm(j,i) /= 0) Then
                  If (coord_info%Dist(j,i) < rmin) Then
                     rmin = coord_info%Dist(j,i)
                     first_neighbor(i) = coord_info%N_CooAtm(j,i)
                  End If
               End If
            End Do
            If (first_neighbor(i) == 0) Then
               Err_CFML%Ierr=1
               Err_CFML%Msg = " => Error! Coordination for atom: "//A%Atom(i)%ChemSymb//" is zero. Increase Dmax!"
               Call Set_Eps_Math(old_eps)  !Restore previous value of epss
               Return
            End If
            Write(unit=lun,fmt="(tr4,a4,tr2,a4,tr7,f6.3)") A%Atom(i)%Lab,A%Atom(first_neighbor(i))%Lab,rmin
         End Do
         Write(unit=lun,fmt="(tr4,a,/)") "-----------------------"

         ! Setting anion/cation nature

         Call Set_Pauling_Electronegativity()

         Allocate(iontype(A%natoms))

         iontype(:) = 0

         ! Ions with anion_candidate = 0 be cations
         ! Ions with anion_candidate greater than zero can be cations or anions
         !     - if the first neighbor anion_candidate = 0, it is an anion
         !     - if the first neighbor anion_candidate /= 0,
         !          it is a cation if it has lower electronegativity
         !          it is an anion otherwise

         Do i = 1 , A%natoms
            If (anion_candidate(i) == 0) Then
               iontype(i) = 1
            Else
               If (anion_candidate(first_neighbor(i)) == 0) Then
                  iontype(i) = -1
               Else
                  If (PaulingX(A%Atom(i)%Z) < PaulingX(A%Atom(first_neighbor(i))%Z)) Then
                     iontype(i) = 1
                  Else
                     iontype(i) = -1
                  End If
               End If
            End If
         End Do

         Write(unit=lun,fmt="(a,/)") " => Anion / Cation assignment...."
         Write(unit=lun,fmt="(tr4,a)") "----------------"
         Write(unit=lun,fmt="(tr4,a)") "Anions / Cations"
         Write(unit=lun,fmt="(tr4,a)") "----------------"

         Do i = 1 , A%natoms
            If (iontype(i) == 1) Then
               Write(unit=lun,fmt="(tr4,a4,tr6,a)") A%Atom(i)%Lab, "CATION"
            Else If (iontype(i) == -1) Then
               Write(unit=lun,fmt="(tr4,a4,tr6,a)") A%Atom(i)%Lab, " ANION"
            End If
         End Do

         Write(unit=lun,fmt="(tr4,a,/)") "----------------"

         ! We check every first-neighbor pair is a cation-anion pair

         Do i = 1 , A%natoms
            If (iontype(i) == iontype(first_neighbor(i))) Then
               error_pairs=.True.
               If (iontype(i) == 1) Then
                  Write(unit=lun,fmt="(a)") " => Error! The pair "//Trim(A%Atom(i)%Lab)//"-"&
                       //Trim(A%Atom(first_neighbor(i))%Lab)//" is a CATION-CATION pair"
               Else
                  Write(unit=lun,fmt="(a)") " => Error! The pair "//Trim(A%Atom(i)%Lab)//"-"&
                       //Trim(A%Atom(first_neighbor(i))%Lab)//" is an ANION-ANION pair"
               End If
            End If
         End Do

         If (error_pairs) Then
            Err_CFML%Ierr=1
            Err_CFML%Msg = " => Error in first-neighbor:  Cation-Cation or Anion-Anion first-neighbor pairs have been found"
            Call Set_Eps_Math(old_eps)  !Restore previous value of epss
            Return
         End If

         ! We check all atoms of a given element are all cations or all anions

         Allocate(elem(nelements))

         Do i = 1 , nelements
            j = 1
            Do
               If (atom2elem(j) == i) Then
                  elem(i)%Z        = A%Atom(j)%Z
                  elem(i)%Lab      = A%Atom(j)%Lab
                  elem(i)%typel    = iontype(j)
                  elem(i)%ChemSymb = A%Atom(j)%ChemSymb
                  Exit
               Else
                  j = j + 1
               End If
            End Do
            k = j + 1
            Do l = k , A%Natoms
               If (atom2elem(l) == i .And. iontype(l) /= iontype(j)) Then
                  Write(unit=lun,fmt="(2(a,i4),a)") " => Error! Atom ", j, " and Atom ", l, " must be both cations or anions"
                  error_element = .True.
               End If
            End Do
         End Do

         If (error_element) Then
            Err_CFML%Ierr=1
            Err_CFML%Msg = " => Error in element:  Cations and anions found for an element"
            Call Set_Eps_Math(old_eps)  !Restore previous value of epss
            Return
         End If

         ! We extract the possible valences for each element

         Allocate(nvalences(nelements))
         Allocate(valence(1:11,nelements))
         nvalences = 0

         Do i = 1 , nelements
            j = 1
            Do
               If (j > 11) Exit
               If (OxStates_Table(j,elem(i)%Z) == 0) Exit

               If (elem(i)%typel == 1 .And. OxStates_Table(j,elem(i)%Z) > 0) Then
                  nvalences(i)            = nvalences(i) + 1
                  valence(nvalences(i),i) = OxStates_Table(j,elem(i)%Z)
               Else If (elem(i)%typel == -1 .And. OxStates_Table(j,elem(i)%Z) < 0) Then
                  nvalences(i)            = nvalences(i) + 1
                  valence(nvalences(i),i) = OxStates_Table(j,elem(i)%Z)
               End If
               j = j + 1
            End Do
            If (nvalences(i) == 0) Then ! He, some noble gas elements
               nvalences(i) = 1
               valence(1,i) = 0
            End If
         End Do

         Write(unit=lun,fmt="(a,/)") " => Extracting valences from Oxidation States Table...."
         Write(unit=lun,fmt="(tr4,a)") "----------------------------------------------"
         Write(unit=lun,fmt="(tr4,a)") "Ion, Usual / (Reported) valences"
         Write(unit=lun,fmt="(tr4,a)") "----------------------------------------------"

         Do i = 1 , nelements
            If (elem(i)%Z == 8) nvalences(i) = 1
            Write(unit=lun,fmt="(tr4,a4)",advance='no') elem(i)%ChemSymb
            Do j = 1 , nvalences(i)
               k = 1
               usual = .False.
               Do !While (.Not. usual .And. k <= 8)
                  If (valence(j,i) == Common_OxStates_Table(k,elem(i)%Z)) usual = .True.
                  k = k + 1
                  If (usual .Or. k > 8) Exit
               End Do
               If (usual) Then
                  Write(unit=lun,fmt="(tr3,i2,tr1)",advance='no') valence(j,i)
               Else
                  Write(unit=lun,fmt="(tr2,a1,i2,a1)",advance='no') "(", valence(j,i), ")"
               End If
            End Do
            Write(unit=lun,fmt=*)
         End Do

         Write(unit=lun,fmt="(tr4,a,/)") "----------------------------------------------"

         Write(unit=lun,fmt="(a,/)") " => Searching for combinations that fulfill the electroneutrality rule..."

         ncombinations=1

         Do i = 1 , nelements
            ncombinations = ncombinations * nvalences(i)
         End Do

         Allocate(solution(nelements,ncombinations),combination(nelements,ncombinations))
         Allocate(contador(nelements))

         nsolutions  = 0
         contador(:) = 1
         icomb       = 1

         Do icomb = 1 , ncombinations
            total_charge = 0.
            Do j = 1 , A%natoms
               combination(atom2elem(j),icomb) = valence(contador(atom2elem(j)),atom2elem(j))
               total_charge = total_charge + valence(contador(atom2elem(j)),atom2elem(j)) * A%atom(j)%occ
            End Do

            If (Abs(total_charge) < ZEROCHARGE) Then
               nsolutions = nsolutions + 1
               Do j = 1 , A%Natoms
                  solution(atom2elem(j),nsolutions) = valence(contador(atom2elem(j)),atom2elem(j))
               End Do
            End If
            incremento = .False.
            j = nelements

            Do !While (.Not. incremento .And. icomb < ncombinations)
               If (incremento .Or. icomb == ncombinations) Exit
               If (contador(j) < nvalences(j)) Then
                  contador(j) = contador(j) + 1
                  incremento  = .True.
               Else
                  contador(j) = 1
                  j = j - 1
               End If
            End Do
         End Do

         If (nsolutions == 0) Then

            Write(unit=lun,fmt="(a)") "    No solution found with integer valences "
            Write(unit=lun,fmt="(a)") "    Trying solutions with non-integer valences (mixed valence)"
            Write(unit=lun,fmt="(a)")

            ! We try with mixed valences (non-integer valences) for transition and earth-rare metals, if any

            Allocate(elemmix(nelements))

            nelemmix   = 0
            elemmix(:) = 0

            Do i = 1 , nelements
               If ( (elem(i)%Z > 20 .And. elem(i)%Z < 31) .Or. &
                    (elem(i)%Z > 38 .And. elem(i)%Z < 49) .Or. &
                    (elem(i)%Z > 56 .And. elem(i)%Z < 81) .Or. &
                    (elem(i)%Z > 88)) Then
                  nelemmix   = nelemmix + 1
                  elemmix(i) = nelemmix
               End If
            End Do

            If (nelemmix == 0) Then
               Err_CFML%Ierr=1
               Err_CFML%Msg = " => No transition / earth-rare ion found. Unable to set charges"
               Call Set_Eps_Math(old_eps)  !Restore previous value of epss
               Return
            Else
               norm = 0.
               nsolutions = ncombinations * nelemmix

               If (nelemmix > 1) Then
                  Deallocate(solution)
                  Allocate(solution(nelements,nsolutions))
               End If

               nsolutions = 0

               Do n = 1 , nelemmix
                  Do i = 1 , A%Natoms
                     If (elemmix(atom2elem(i)) == n ) norm = norm + A%Atom(i)%occ
                  End Do

                  Do i = 1 , ncombinations
                     total_charge = 0.
                     Do j = 1 , A%Natoms
                        If (elemmix(atom2elem(j)) /= n) &
                             total_charge  = total_charge + combination(atom2elem(j),i) * A%Atom(j)%occ
                     End Do
                     Do j = 1 , nelements
                        If (elemmix(j) /= n) Then
                           solution(j,nsolutions+1) = combination(j,i)
                        Else
                           solution(j,nsolutions+1) = - total_charge / norm
                        End If
                     End Do

                     ! check if this solution is new

                     newsolution = .True.
                     m  = 1

                     Do !While (newsolution .And. m <= nsolutions)
                        If (newsolution .Or. m > nsolutions) Exit
                        identical = .True.

                        Do k = 1 , nelements
                           If (solution(k,nsolutions+1) /= solution(k,m)) identical = .False.
                        End Do

                        If (identical) newsolution = .False.
                        m = m + 1
                     End Do

                     If (newsolution) nsolutions = nsolutions + 1
                  End Do
               End Do
            End If
         End If

         If (nsolutions == 0) Then
            Err_CFML%Ierr=1
            Err_CFML%Msg = " => No solution found. Unable to set charges"
            Call Set_Eps_Math(old_eps)  !Restore previous value of epss
            Return
          End If

         ! We compute how many unusual valences are in each solution, and set a penalty according to this

         Allocate(penalty(nsolutions))
         Allocate(nunusual(nsolutions))

         nunusual(:)    = 0
         penalty(:)     = 0.

         Do i = 1 , nsolutions
            Do j = 1 , nelements
               k = 1
               usual = .False.
               Do !While (.Not. usual .And. k <= 8)
                  If (solution(j,i) == Common_OxStates_Table(k,elem(j)%Z)) usual = .True.
                  k = k + 1
                  If (usual .Or. k > 8) Exit
               End Do
               If (.Not. usual) Then
                  nunusual(i)    = nunusual(i) + 1
                  ! Compute penalization
                  p = 100
                  k = 1
                  Do
                     If (k > 8) Exit
                     If (Common_OxStates_Table(k,elem(j)%Z) == 0) Exit
                     If (elem(j)%typel == 1 .And. Common_OxStates_Table(k,elem(j)%Z) > 0) Then
                        diff = Abs(solution(j,i) - Common_OxStates_Table(k,elem(j)%Z))
                        If (p > diff) p = diff
                     Else If (elem(j)%typel == -1 .And. Common_OxStates_Table(k,elem(j)%Z) < 0) Then
                        diff = Abs(solution(j,i) - Common_OxStates_Table(k,elem(j)%Z))
                        If (p > diff) p = diff
                     End If
                     k = k + 1
                  End Do
                  penalty(i) = penalty(i) + p
               End If
            End Do
         End Do

         penalty_min = Minval(penalty)
         nsol_usual = 0.

         Do i = 1 , nsolutions
            If (nunusual(i) == 0) nsol_usual = nsol_usual + 1
         End Do

         Write(unit=lun,fmt="(tr4,i10,a)") nsolutions, " solutions found"
         Write(unit=lun,fmt="(tr4,i10,a,/)") nsol_usual, " solutions with usual formal charges (penalty = 0)"

         If (nsolutions > massive_limit) Then
            Write(unit=lun,fmt="(tr4,a)") "number of solutions too large"
            Write(unit=lun,fmt="(tr4,a,/)") "work only with solutions with the minimum penalty"

            npenalty_min = 0
            Allocate(solution_aux(nelements))

            Do i = 1 , nsolutions

               If (penalty(i) == penalty_min) Then
                  npenalty_min = npenalty_min + 1
                  solution_aux(:) = solution(:,i)
                  solution(:,npenalty_min) = solution_aux(:)
               End If

            End Do

            Deallocate(solution_aux)
            Allocate(solution_new(nelements,npenalty_min))
            solution_new(:,:) = solution(:,1:npenalty_min)
            Deallocate(solution)
            Allocate(solution(nelements,npenalty_min))
            solution(:,:) = solution_new(:,:)
            Deallocate(solution_new)
            Deallocate(penalty)
            Allocate(penalty(npenalty_min))
            penalty(:) = penalty_min
            nsolutions = npenalty_min
         End If

         ! We sort solutions according to their penalty

         Allocate(solution_aux(nelements))
         ordered = .False.

         Do !While (.Not. ordered)
            ordered = .True.
            Do i = 1 , nsolutions - 1
               If (penalty(i+1) < penalty(i)) Then
                  ordered = .False.
                  penalty_aux     = penalty(i+1)
                  nunusual_aux    = nunusual(i+1)
                  solution_aux(:) = solution(:,i+1)
                  penalty(i+1)    = penalty(i)
                  nunusual(i+1)   = nunusual(i)
                  solution(:,i+1) = solution(:,i)
                  penalty(i)      = penalty_aux
                  nunusual(i)     = nunusual_aux
                  solution(:,i)   = solution_aux(:)
               End If
            End Do
            If (ordered) Exit
         End Do

         ! Compute Energies

         Write(unit=lun,fmt="(a,/)") " => Computing energies..."

         Allocate(energy(nsolutions))
         energy=0.0
         Do i = 1 , nsolutions
            ! Assign charges to every atom and compute the energy
            k = 0
            Bc%Charge(:) = 0
            Do j = 1 , A%Natoms
               Do m = 1 , A%Atom(j)%Mult
                  k = k + 1
                  Bc%Charge(Ac2Bc(k)) =  Bc%Charge(Ac2Bc(k)) + solution(atom2elem(j),i) * A%Atom(j)%Occ / sumocc(Ac2Bc(k))
               End Do
            End Do
            Call Ewald(Cell%CR_Orth_Cel,Cell%Vol,Bc,energy(i))
         End Do
         ! For each penalty value, we sort solutions according to their energy
         m = 1
         Do !While (m < nsolutions)
            If (m >= nsolutions) Exit
            n = m
            j = m + 1
            Do       !While (j <= nsolutions)
               If (j > nsolutions) Exit
               If (penalty(m) == penalty(j)) n = n + 1
               j = j + 1
            End Do
            If (n /= m) Then
               ordered = .False.
               Do     !While (.Not. ordered)
                  ordered = .True.
                  Do i = m , n - 1
                     If (energy(i+1) < energy(i)) Then
                        ordered         = .False.
                        energy_aux      = energy(i+1)
                        nunusual_aux    = nunusual(i+1)
                        solution_aux(:) = solution(:,i+1)
                        energy(i+1)     = energy(i)
                        nunusual(i+1)   = nunusual(i)
                        solution(:,i+1) = solution(:,i)
                        energy(i)       = energy_aux
                        nunusual(i)     = nunusual_aux
                        solution(:,i)   = solution_aux(:)
                     End If
                  End Do
                  If (ordered) Exit
               End Do
               m = n
            Else
               m = m + 1
            End If
         End Do

         Write(unit=lun,fmt="(tr5)",advance="no")
         Do i = 1 , nelements
            Write(unit=lun,fmt="(a)",advance='no') "---------"
         End Do
         Write(unit=lun,fmt="(a)") "---------------------"
         Do i = 1 , nelements
            Write(unit=lun,fmt="(tr5,a4)",advance="no") elem(i)%ChemSymb
         End Do
         Write(unit=lun,fmt="(a)") "  Energy (eV/atom) Penalty"
         Write(unit=lun,fmt="(tr5)",advance="no")
         Do i = 1 , nelements
            Write(unit=lun,fmt="(a)",advance='no') "---------"
         End Do
         Write(unit=lun,fmt="(a)") "---------------------"

         Do i = 1 , nsolutions
            Write(unit=lun,fmt="(tr4)",advance="no")
            Do j = 1 , nelements
               Write(unit=lun,fmt="(f5.2,tr4)",advance='no') solution(j,i)
            End Do
            Write(unit=lun,fmt="(tr5,f8.3,i9)") energy(i), penalty(i)
         End Do

         Write(unit=lun,fmt="(tr5)",advance="no")

         Do i = 1 , nelements
            Write(unit=lun,fmt="(a)",advance='no') "---------"
         End Do
         Write(unit=lun,fmt="(a)") "---------------------"
         Write(unit=lun,fmt="()")

         ! Determine the number of solutions with the minimum penalty and the same energy
         !         minimum = penalty(1)

         nequiv_sol = 1
         Do i = 2 , nsolutions
            If (penalty(i) == penalty(1) .And. Abs(energy(i) - energy(1)) < 0.001) nequiv_sol = nequiv_sol + 1
         End Do

         If (nequiv_sol > 1) Then
            Write(unit=lun,fmt="(tr5,i2,a)") nequiv_sol, " equivalent solutions found."
            Write(unit=lun,fmt="(tr8,a,/)") "final solution = average of equivalent solutions"
         End If

         warning_valence = .False.
         Do i = 1 , A%Natoms
            A%Atom(i)%Charge = 0
            Do j = 1 , nequiv_sol
               A%Atom(i)%Charge = A%Atom(i)%Charge + solution(atom2elem(i),j)
            End Do
            A%Atom(i)%Charge = A%Atom(i)%Charge / nequiv_sol
            If (Abs(Abs(A%Atom(i)%Charge) - Abs(A%Atom(i)%Charge)) > 0) warning_valence = .True.
         End Do

      End If

      If (warning_valence) Then
         Write(unit=lun,fmt="(a,/)") " => WARNING: Non-integer valences found"
      End If
      Call Set_Eps_Math(old_eps)  !Restore previous value of epss

    End Subroutine Set_Formal_Charges

    !!----
    !!---- Module Subroutine Calc_Site_Ene(A,Spg,Cell,x,y,z,atname,drmax,emin)  ! added by Nebil 14-09-2016
    !!----    !---- Arguments ----!
    !!----    type (Atoms_Conf_List_type), intent(in) :: A
    !!----    type (Space_Group_Type),     intent(in) :: SpG
    !!----    Type (Crystal_Cell_Type),    intent(in) :: Cell
    !!----    real(kind=cp),               intent(in) :: x
    !!----    real(kind=cp),               intent(in) :: y
    !!----    real(kind=cp),               intent(in) :: z
    !!----    character(len=*),            intent(in) :: atname  !Species of the probe atom e.g. LI+1
    !!----    real(kind=cp),               intent(in) :: drmax
    !!----    real(kind=cp),               intent(out):: emin
    !!----
    !!----    Calculate Bond-Valence site energy where the energy at point (x,y,z)
    !!----    is determined by a species representative defined in atname. The BV-site Energy value
    !!----    is evaluated for distances below drmax value.
    !!----
    !!---- Created: September 2016, added to crysfml April 2021
    !!
    Module Subroutine Calc_Site_Ene(A,Spg,Cell,x,y,z,atname,drmax,emin)
      !---- Arguments ----!
      Type (Atoms_Conf_List_Type), Intent(in out):: A
      class (SpG_Type),            Intent(in)    :: SpG
      Type (Cell_G_Type),          Intent(in)    :: Cell
      real(kind=cp),               intent(in)    :: x
      real(kind=cp),               intent(in)    :: y
      real(kind=cp),               intent(in)    :: z
      character(len=*),            intent(in)    :: atname  !Species of the probe atom e.g. LI+1
      real(kind=cp),               intent(in)    :: drmax
      real(kind=cp),               intent(out)   :: emin

      !---- Local variables ----!
      character(len=4)                           :: car,atm
      integer                                    :: i,j,n,n1,n2 !,k
      integer                                    :: nx1,nx2,ny1,ny2,nz1,nz2
      integer                                    :: i1,j1,k1,sig1,sig2,ncont
      real(kind=cp)                              :: rx1,ry1,rz1,qval,q1,q2,rep
      real(kind=cp)                              :: sbvs, dd, occ, radius, rho, dmin, d_cutoff, &
                                                    dzero, alpha,c_rep,c_atr
      real(kind=cp), dimension(3)                :: pto,pta,extend
      Type (AtList_Type)                         :: At1, At2
      logical                                    :: anion,cation
      !Coulomb constant (1/4pie0) to get energies in eV, use electron charges and distances in angstroms
      real(kind=dp), parameter :: ke=14.399644850445155254866066
      !Principal quantum numbers of the test=ion  and the species of all the atoms of the list
      real(kind=cp) :: n_tion, ferfc
      real(kind=cp), dimension(:), allocatable :: n_j

      !---- Initial conditions ----!
      if (A%natoms <= 0) return
      call clear_error()

      !---- Preparing the Variables and calculations ----!
      call Allocate_Atom_List(A%natoms,At1,"Atm_Std_type",0)
      At1%atom=A%atom  !Atom list A%Atom(:)%ind_ff(1) contains the pointer to the species
                       !coming from A (Atoms_Conf_List_type, set in the main program)
      atm=u_case(atname)
      n1=0
      allocate(n_j(A%N_Spec))
      n_j=0.0
      if (.not. allocated(Ap_Table)) call Set_Atomic_Properties_Table()
      do j=1,A%N_Spec
        car=u_case(A%species(j))
        do i=1,Ap_species_n
          if(Ap_Table(i)%Symb == car) then
             n_j(j)=real(Ap_Table(i)%n)
             exit
          end if
        end do
        if(car == atm) n_tion=n_j(j)
      end do

      do i=1,At1%natoms  !This exclude all atoms with the same species as Atname
         n2=A%Atom(i)%ind_ff(1)
         if (trim(u_case(A%species(n2))) == trim(atm)) then
            At1%atom(i)%active=.false.
            q1=At1%atom(i)%charge
            sig1=nint(SIGN(1.0_cp,q1))
            n1=n2
            radius=A%radius(n1)
            car=A%Species(n1)
         end if
      end do
      if (n1 == 0) then
         Err_CFML%Ierr=1
         Err_CFML%Msg = "The point atom "//atname//" is not in the Species Table"
      end if

      call Extend_Atom_List(At1,At2,Spg,"Atm_Std",.true.)  !Here the non-active atoms of At2 are not in At2

      if(Err_CFML%Ierr /= 0) then
        write(unit=*,fmt="(a)") " =>"//"Error in Calc_Site_Energy: "//trim(Err_CFML%Msg)
        return
      end if
      !call AtList1_ExtenCell_AtList2(Spg,At1,At2,.true.)
      !call Deallocate_atom_list(At1)
      !check that all species are well set in the list

      !Write(unit=*,fmt="(a)") " => List of atoms for calculating BVEL (Calc_Site_Ene)"
      !do n=1,At2%natoms
      !    n2=At2%Atom(n)%ind_ff(1)
      !    write(*,"(a30,3f10.5,2i6)") trim(At2%atom(n)%Lab)//"  "//At2%atom(n)%ChemSymb//"  ", At2%atom(n)%x, At2%atom(n)%charge, At2%atom(n)%ind_ff(1)
      !end do

      do n=1,At2%natoms
          n2=At2%Atom(n)%ind_ff(1)
          !write(unit=*,fmt="(a,a,3f12.5,f8.4)") At2%Atom(n)%Lab,At2%Atom(n)%SfacSymb,At2%Atom(n)%x,real(At2%Atom(n)%charge)
          if (n2 == 0) then
             Err_CFML%Ierr=1
             Err_CFML%Msg = "The atom "//trim(At2%Atom(n)%lab)//" is not in the Species Table"
             return
          end if
      end do

      ! Determination of the valence expected for a good position
      i=index(car,"+")
      cation=.false.
      anion=.false.
      if( i /= 0) then
        read(unit=car(i+1:),fmt=*) qval
        cation=.true.
      else
        i=index(car,"-")
        read(unit=car(i+1:),fmt=*) qval
        anion=.true.
      end if
      extend=[ drmax/cell%cell(1),  drmax/cell%cell(2), drmax/cell%cell(3) ]

      pto(1)=x
      pto(2)=y
      pto(3)=z

      rx1=pto(1)-extend(1)
      if (rx1 <= 0.0) then
         nx1=int(rx1)-1
      else
         nx1=int(rx1)
      end if
      nx2=int(pto(1)+extend(1))

      ry1=pto(2)-extend(2)
      if (ry1 <= 0.0) then
         ny1=int(ry1)-1
      else
         ny1=int(ry1)
      end if
      ny2=int(pto(2)+extend(2))

      rz1=pto(3)-extend(3)
      if (rz1 <= 0.0) then
         nz1=int(rz1)-1
      else
         nz1=int(rz1)
      end if
      nz2=int(pto(3)+extend(3))

      sbvs=0.0
      rep=0.0
      ncont=0
      !write(*,"(a,6i4)") "nz1,nz2,ny1,ny2,nx1,nx2: ",nz1,nz2,ny1,ny2,nx1,nx2
      !write(*,"(a,3f10.5,a,f10.4)") "Coordinates of the site: ",pto, "  drmax: ",drmax
      do n=1,At2%natoms
         n2=At2%Atom(n)%ind_ff(1)
         q2=At2%Atom(n)%charge
         sig2= nint(SIGN(1.0_cp,q2))
         rho=(radius+A%radius(n2))*0.74
         dzero=Table_Dzero(n1,n2)
         dmin=Table_Rmin(n1,n2)
         alpha=Table_Alpha(n1,n2)
         d_cutoff=Table_Rcutoff(n1,n2)
         occ=At2%Atom(n)%VarF(1)
         c_rep=occ*q1*q2/sqrt(n_tion*n_j(n2))
         c_atr=occ*dzero
         !write(*,"(2i4,12f10.4)") n1,n2,q1,q2,rho,dzero,dmin,alpha,d_cutoff,occ,n_tion,n_j(n2), c_rep, c_atr
         ferfc=erfc(drmax/rho)/drmax !always below 10^(-9) when drmax/rho > 4.2
         do k1=nz1,nz2
            do j1=ny1,ny2
               do i1=nx1,nx2
                  pta=At2%Atom(n)%x+real([i1,j1,k1])
                  dd=max(Distance(pto,pta,Cell),0.0001) !To avoid division by zero
                  !write(*,"(a,i5,3i4,3f10.4,f12.4)") " n,i1,j1,k1,pta,distance: ",n,i1,j1,k1,pta,dd
                  if (dd > drmax) cycle
                  if (sig1 == sig2) then
                     rep=rep + c_rep*(erfc(dd/rho)/dd-ferfc)
                  else
                     !if(dd > d_cutoff) cycle
                     sbvs=sbvs+ c_atr*((exp(alpha*(dmin-dd))-1.0)**2-1.0)
                  end if
                  !write(*,"(a,3f10.4)") " rep,sbvs,distance: ",rep,sbvs,dd
               end do
            end do
         end do

      end do
      !Multiply the repulsion term by the Coulomb constant to convert to eV
      emin=sbvs+ke*rep
      !write(*,"(a,2f10.4)")  " => Drmax and Site Energy",drmax,emin

    End Subroutine Calc_Site_Ene


 End Submodule EnBVS_Energy
