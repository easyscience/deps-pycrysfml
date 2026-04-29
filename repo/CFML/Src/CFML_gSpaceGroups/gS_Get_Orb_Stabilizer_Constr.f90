!!----
SubModule (CFML_gSpaceGroups) gS_Get_Orb_Stabilizer_Constr
    implicit none

    character (len=*), dimension(26),parameter   :: &
    cdd=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r', &
         's','t','u','v','w','x','y','z']
   Contains

   !!----
   !!---- GET_STABILIZER
   !!----    Subroutine to obtain the list of symmetry operators of a space group that leaves
   !!----    invariant an atomic position. This subroutine provides a pointer to the symmetry
   !!----    operators of the site point group and the additional translation with respect to
   !!----    the canonical representative.
   !!----
   !!---- 13/06/2019
   !!
   Module Subroutine Get_Stabilizer(X, Spg,Order,Ptr,Atr)
      !---- Arguments ----!
      real(kind=cp), dimension(3),  intent (in)  :: x     ! real space position (fractional coordinates)
      class(Spg_Type),              intent (in)  :: Spg   ! Space group
      integer,                      intent(out)  :: order ! Number of sym.op. keeping invariant the position x
      integer, dimension(:),        intent(out)  :: ptr   ! Array pointing to the symmetry operators numbers
                                                          ! of the stabilizer of x
      real(kind=cp), dimension(:,:),intent(out)  :: atr   ! Associated additional translation to the symmetry operator

      !---- Local variables ----!
      real(kind=cp), dimension(3)    :: xx, tr
      integer                        :: j,n1,n2,n3 !,nop

      !> Init
      order = 1              ! Identity belongs always to the stabilizer
      ptr   = 0; ptr(1)=1
      atr   = 0.0_cp
      !nop=min(2*Spg%NumOps,Spg%Multip)
       do n1=-1,1
          do n2=-1,1
             do n3=-1,1
               tr=real([n1, n2, n3])
               do j=2,Spg%Multip
                  xx=Apply_OP(Spg%Op(j),x)  - x   + tr
                  if (sum(abs(xx)) > 2.0 * EPSS) cycle
                  order=order+1
                  ptr(order)=j
                  atr(:,order)=tr
               end do
             end do
          end do
       end do

   End Subroutine Get_Stabilizer

   !! Module Subroutine Get_Orbit(x,Spg,orbit,Lab,Chem,mom,orb3D,convl,Tbox)
   !!
   !!---- Subroutine to generate the orbit of a point x in space/superspace
   !!---- If mom is not present then the orbit%mom components remain unallocated
   !!---- If orb3D is present, the restriction to 3D space is also output.
   !!---- If convl is present and convl=.false., only the orbit within a primitive
   !!---- cell is output. If Tbox is present, the orbit is extended to a supercell
   !!---- that is [ Tbox(1) x a, Tbox(2) x b, Tbox(3) x c ]  by adding all possible
   !!---- translations to the Zero-cell orbit
   !!----
   !!---- The orb object is of class Point_Orbit or Orbit_Type defined in the general module
   !!---- and repeated here for the sake of proper documentation:
   !!
   !!     Type, public :: Point_Orbit
   !!        integer                                      :: Mult=0 ! Multiplicity of the orbit
   !!        real(kind=cp),allocatable,dimension(:,:)     :: pos    ! (d,Mult) Positions of the points
   !!        real(kind=cp),allocatable,dimension(:,:)     :: mom    ! (d,Mult) Associated moments
   !!        integer,      allocatable,dimension(:)       :: pts    ! (  Mult) Pointer to symmetry operator
   !!        integer,      allocatable,dimension(:,:)     :: Lat    ! (d,Mult) lattice translation used to
   !!     End Type Point_Orbit                                      ! put the atom  g(i).pos(:,1) within the cell
   !!                                                               ! pos(:,i)=  g(pts(i)).pos(:,1) + Lat(:,i)
   !!     Type, extends(Point_Orbit),public :: Orbit_type
   !!        character(len=2)                               :: ChemSymb  ! Chemical symbol
   !!        character(len=20), dimension(:),   allocatable :: lab       ! Atom label
   !!        integer,           dimension(:),   allocatable :: Ls        ! Pointer to a lattice translation
   !!        integer,           dimension(:,:), allocatable :: Latt      ! Additional lattice translation to the operator
   !!     End Type Orbit_type                                            ! put the atom  g(i).pos(:,1) within the cell
                                                                   ! pos(:,i)=  g(pts(i)).pos(:,1) + Lat(:,i)
   !!
   Module Subroutine Get_Orbit(x,Spg,orbit,Lab,Chem,mom,orb3D,convl,Tbox)
      !---- Arguments ----!
      real(kind=cp), dimension(:),          intent(in)  :: x
      class(SpG_Type),                      intent(in)  :: spg
      class(Point_Orbit),                   intent(out) :: orbit
      character(len=*),           optional, intent(in)  :: Lab,Chem
      real(kind=cp), dimension(:),optional, intent(in)  :: mom
      class(Point_Orbit),         optional, intent(out) :: orb3D
      logical,                    optional, intent(in)  :: convl
      integer,       dimension(3),optional, intent(in)  :: Tbox

      !---- Local variables ----!
      integer                                          :: i, j, n, nt,d,mult,n_tr,L,i1,i2,i3
      real(kind=cp), dimension(Spg%d)                  :: xs,xsp
      real(kind=cp), dimension(Spg%d)                  :: ms,msp
      real(kind=cp), dimension(Spg%d-1)                :: v
      real(kind=cp), dimension(Spg%d,Spg%d,Spg%multip) :: Om
      real(kind=cp), dimension(Spg%d-1,Spg%multip)     :: Orb,morb
      real(kind=cp), dimension(:,:), allocatable       :: OrbE
      character(len=2)                                 :: ChemS
      character(len=15)                                :: Label
      character(len=15), dimension(Spg%multip)         :: LabS
      integer,       dimension(:,:), allocatable       :: LatE
      integer,       dimension(Spg%d-1)                :: Latt
      integer,       dimension(Spg%d-1,Spg%multip)     :: Lat
      integer,       dimension(:),  allocatable        :: ptr
      logical                                          :: conv, orbit_full
      integer,       dimension(:,:), allocatable       :: add_Lat
      real(kind=cp), dimension(Spg%d)                  :: momd

      conv=.true.
      if(present(convl)) conv=convl
      d=Spg%d-1; orbit_full=.false.
      xs(Spg%d)=1.0_cp
      ms=0.0
      ms(Spg%d)=1.0_cp
      orb=0.0_cp; morb=0.0_cp
      momd=0.0; momd(Spg%d)=1.0_cp
      if(present(mom)) momd(1:3)=mom(1:3)
      if(present(Lab) .or. present(Chem)) then
        Select Type(orbit)
          Type is (Orbit_type)
            orbit_full=.true.
            if(present(Chem)) then
              ChemS=Chem
            else
              ChemS=Get_Chem_Symb(Lab)
            end if
          Type is (Point_Orbit)
            err_cfml%ierr = 1
            err_cfml%flag = .true.
            err_cfml%msg = 'When Labels and Chemical symbol are used, the provided Orbit object class should be of type: Orbit_type'
            return
        End Select
      end if

      do i=1,Spg%Multip
        Om(:,:,i)=Spg%Op(i)%Mat
      end do

      Select Type(SpG)

        type is (SuperSpaceGroup_Type)
           xs(1:3)=x   !Extend the position and moment to superspace
           ms(1:3) = momd(1:3)
           do i=1,SpG%nk
             xs(3+i)=dot_product(x,SpG%kv(:,i))
             ms(3+i)=dot_product(momd(1:3),SpG%kv(:,i))
           end do
        class default
           xs(1:d)=x(1:d)
           ms(1:d) = momd(1:d)
      End Select

      allocate(ptr(Spg%multip))
      mult=1
      call Lat_Modulo(xs(1:d),v(1:d),Latt)
      orb(:,1)=v(1:d)
      Lat(:,1)=Latt
      ptr(mult) = 1
      morb(1:d,1)=ms(1:d)

      do_ext: do j=2,Spg%Multip
         xsp=matmul(Om(:,:,j),xs)
         call Lat_Modulo(xsp(1:d),v(1:d),Latt)
         xsp(1:d)=v(1:d)
         do nt=1,mult
            v(1:d)=orb(1:d,nt)-xsp(1:d)
            if(sum(abs(v(1:d))) < 2.0 * EPSS) cycle do_ext
            if(Spg%Num_lat > 0 .and. .not. conv) then
              if (is_Lattice_vec(v(1:d),Spg%Lat_tr(1:d,:))) cycle do_ext
            else
              if (Zbelong(v(1:d))) cycle do_ext
            end if
         end do
         msp(1:d)=Spg%Op(j)%dt*Spg%Op(j)%time_inv*matmul(Om(1:d,1:d,j),ms(1:d))
         mult=mult+1
         if(orbit_full) then
           label=" "
           write(unit=label,fmt="(a,i3)") trim(lab)//"_",mult
           labS(mult)=pack_string(label)
         end if
         orb(1:d,mult)=xsp(1:d)
         Lat(1:d,mult)=Latt
         morb(1:d,mult)=msp(1:d)
         ptr(mult) = j   !Pointer to symmetry operator
      end do do_ext

      if(present(Tbox)) then  !Add supplemental cells
        n_tr=tbox(1)*tbox(2)*tbox(3); L=0
        allocate(add_Lat(d,n_tr))
        add_Lat=0
        do i1=0,tbox(1)-1
          do i2=0,tbox(2)-1
             do i3=0,tbox(3)-1
                if(i1 == 0 .and. i2 == 0 .and. i3 == 0 ) cycle
                L=L+1
                add_Lat(1:3,L)=[i1,i2,i3]
             end do
          end do
        end do
        n_tr=L
        nt=mult*(n_tr+1)
        orbit%Mult=nt
        allocate(orbit%pos(d,nt), orbit%pts(nt), orbit%Lat(d,nt))
        allocate(orbit%mom(d,nt))
        if(orbit_full) then
          Select Type (Orbit)
            Type is(Orbit_Type)
              allocate(orbit%lab(nt))
              orbit%ChemSymb=ChemS
              orbit%Lab(1:mult)=labS(1:mult)
          End Select
          !write(unit=labS(mult),fmt="(a,i3)") trim(lab)//"_",mult
          !labS(mult)=pack_string(labS(mult))
        end if
        orbit%pos(:,1:mult) = orb(:,1:mult)
        orbit%pts(1:mult)   = ptr(1:mult)
        orbit%Lat(:,1:mult) = Lat(:,1:mult)
        orbit%mom(:,1:mult) = morb(:,1:mult)

        if(orbit_full) then
          nt=mult
          Select Type (Orbit)
            Type is(Orbit_Type)
                do L=1,n_tr
                  do n=1,mult
                    orbit%pos(:,nt+n)=orb(:,n)+ add_Lat(:,L)
                    orbit%Lat(:,nt+n)=Lat(:,n)+ add_Lat(:,L)
                    orbit%pts(nt+n)= ptr(n)
                    orbit%mom(:,nt+n)=morb(:,n)
                    label=" "
                    write(unit=label,fmt="(a,i3)") trim(lab)//"_",nt+n
                    orbit%Lab(nt+n)=pack_string(label)
                  end do
                  nt=nt+mult
                end do
          End Select

        else

          nt=mult
          do L=1,n_tr
            do n=1,mult
              orbit%pos(:,nt+n)=orb(:,n)+ add_Lat(:,L)
              orbit%Lat(:,nt+n)=Lat(:,n)+ add_Lat(:,L)
              orbit%pts(nt+n)= ptr(n)
              orbit%mom(:,nt+n)=morb(:,n)
            end do
            nt=nt+mult
          end do
        end if

      else

         orbit%Mult=mult
         orbit%pos=orb(:,1:mult)   !Automatic allocation
         orbit%pts=ptr(1:mult)
         orbit%Lat=Lat(:,1:mult)
         orbit%mom=morb(:,1:mult)

      end if

      if(present(orb3D)) then !Restriction to 3D
        mult=orbit%Mult
        allocate(orbe(3,Mult),LatE(3,Mult))
        if(allocated(ptr)) deallocate(ptr)
        allocate(ptr(Mult))
        ptr=0;orbe=0.0;late=0
        nt=1
        orbE(:,1)=orb(1:3,1)
        ptr(1)=1
        LatE(:,1)=0
        do_i:do i=2,mult
          do j=1,nt
            if(sum(abs(orbE(:,j)-orbit%pos(1:3,i))) < 2.0 * EPSS ) cycle do_i
          end do
          nt=nt+1
          orbE(:,nt) = orbit%pos(1:3,i)
          LatE(:,nt) = orbit%Lat(1:3,i)
          ptr(nt)=i    !This points to the i-atom of the superspace orbit
        end do do_i
        orb3D%mult=nt

        allocate(orb3D%pos(3,nt),orb3D%Lat(3,nt),orb3D%pts(nt))
        allocate(orb3D%mom(3,nt))
        do i=1,nt
          orb3D%pos(:,i)= orbE(:,i)
          orb3D%Lat(:,i)= LatE(:,i)
          orb3D%pts(i)  = orbit%pts(ptr(i)) !This points to the symmetry operator
          orb3D%mom(:,i)= orbit%mom(1:3,ptr(i))
        end do
        if(orbit_full) then
         Select Type (orb3D)
            Type is(Orbit_Type)
              orb3D%ChemSymb=Chem
              allocate(orb3D%lab(nt))
              do i=1,nt
                label=" "
                write(unit=label,fmt="(a,i3)") trim(lab)//"_",i
                orb3D%lab(i)= pack_string(label)
              end do
         End Select
        end if

      end if

   End Subroutine Get_Orbit

   !>  Module Subroutine Get_Orbit3D(x,Lab,Chem,Spg,orbit,tbox)
   !>     !---- Arguments ----!
   !>     real(kind=cp), dimension(:),          intent(in)  :: x         !Fractional coordinates
   !>     character(len=*),                     intent(in)  :: Lab,Chem  !Label and chemical symbol
   !>     class(SpG_Type),                      intent(in)  :: spg       !Space group object
   !>     type(Orbit_type),                     intent(out) :: orbit     !Output orbit
   !>     integer,       dimension(3),optional, intent(in)  :: tbox      !Box for calculating the orbit
   !>
   !>     This subroutine is similar to Get_Orbit but restricted to 3-dimensions
   !>     It calculates the orbit of an atom within of the average structure in
   !>     case the SpG is an SSG. At variance with Get_Orbit  the label of the input
   !>     atom and its chemical symbol should be provided.See doc of Get_Orbit.
   !>
   !>
   Module Subroutine Get_Orbit3D(x,Lab,Chem,Spg,orbit,tbox,mom)
      !---- Arguments ----!
      real(kind=cp), dimension(:),          intent(in)  :: x
      character(len=*),                     intent(in)  :: Lab,Chem
      class(SpG_Type),                      intent(in)  :: spg
      type(Orbit_type),                     intent(out) :: orbit
      integer,       dimension(3),optional, intent(in)  :: tbox
      real(kind=cp), dimension(:),optional, intent(in)  :: mom

      !---- Local variables ----!
      integer                                          :: j, nt,d,mult,L,mtot,i1,i2,i3,k
      real(kind=cp), dimension(Spg%d)                  :: xs,xsp
      real(kind=cp), dimension(Spg%d-1)                :: v
      real(kind=cp), dimension(Spg%d,Spg%d,Spg%multip) :: Om
      real(kind=cp), dimension(Spg%d-1,Spg%multip)     :: Orb
      integer,       dimension(Spg%d-1)                :: Latt
      integer,       dimension(Spg%d-1,Spg%multip)     :: Lat
      integer,       dimension(:),   allocatable       :: ptr
      integer,       dimension(:,:), allocatable       :: trn
      character(len=20)                                :: label

      d=Spg%d-1
      xs=0.0
      xs(Spg%d)=1.0_cp
      xs(1:3)=x(1:3)
      orb=0.0_cp

      do j=1,Spg%Multip
        Om(:,:,j)=Spg%Op(j)%Mat
      end do
      !Write(*,"(a)") " Operators assigned"
      allocate(ptr(Spg%multip))
      ptr=0
      mult=1
      orb(:,mult)=xs(1:d)
      Lat(:,mult)=0
      ptr(mult) = 1   !Pointer to symmetry operator
      do_ext: do j=2,Spg%Multip
         xsp=matmul(Om(:,:,j),xs)
         call Lat_Modulo(xsp(1:d),v(1:d),Latt)
         xsp(1:d)=v(1:d)
         do nt=1,mult
            v(1:3)=orb(1:3,nt)-xsp(1:3)
            if(sum(abs(v(1:3))) < 1.0 * EPSS) cycle do_ext
         end do
         mult=mult+1
         orb(:,mult)=xsp(1:d)
         Lat(:,mult)=Latt(1:d)
         ptr(mult) = j   !Pointer to symmetry operator
      end do do_ext

      if(present(tbox)) then
         nt=tbox(1)*tbox(2)*tbox(3)
         allocate(trn(3,nt))
         trn=0
         nt=0
         do i1=0,tbox(1)-1
           do i2=0,tbox(2)-1
             do i3=0,tbox(3)-1
               nt=nt+1
               trn(:,nt)=[i1,i2,i3]
             end do
           end do
         end do

         mtot=mult*nt
         orbit%Mult=mtot
         allocate (orbit%pos(3,mtot),orbit%pts(mtot),orbit%lab(mtot),orbit%Lat(3,mtot))
         orbit%pos=0.0; orbit%Lat=0; orbit%lab=" "; orbit%ChemSymb=Chem

         k=0
         do L=1,nt
           do j=1,Mult
             k=k+1
             orbit%pos(1:3,k) = orb(1:3,j) + trn(:,L)
             orbit%pts(    k) = ptr(j)
             orbit%Lat(1:3,k) = Lat(1:3,j) + trn(:,L)
             label=" "
             write(unit=label,fmt="(a,i5)")  trim(Lab)//"_",k
             orbit%lab(k)=pack_string(label)
           end do
         end do
      else
         allocate(orbit%lab(mult))
         do j=1,Mult
           label=" "
           write(unit=label,fmt="(a,i5)")  trim(Lab)//"_",j
           orbit%lab(j)=pack_string(label)
         end do
         orbit%ChemSymb=Chem
         orbit%Mult=mult
         orbit%pos=orb(1:3,1:mult)   !Automatic allocation
         orbit%pts=ptr(1:mult)
         orbit%Lat=Lat(1:3,1:mult)
      end if

   End Subroutine Get_Orbit3D

   !!----
   !!----  Module Subroutine Get_AtomBet_CTR(X,Betas,Spgr,Codini,Icodes,Multip,Ord,Ss,Ipr)
   !!----     real(kind=cp), dimension(3),             intent(in    ) :: X         !Atom position (fractional coordinates)
   !!----     real(kind=cp), dimension(6),             intent(in out) :: Betas     !Anisotropic temperature factors
   !!----     type(SPG_Type),                          intent(in    ) :: Spgr      !Space Group
   !!----     Integer,                                 intent(in out) :: Codini    !Last attributed parameter
   !!----     Integer, dimension(6),                   intent(in out) :: Icodes    !codewords for betas only number
   !!----     real(kind=cp), dimension(6),             intent(in out) :: Multip    !Multipliers
   !!----     integer,                       optional, intent(in    ) :: Ord       !Order of the stabilizer
   !!----     integer, dimension(:),         optional, intent(in    ) :: Ss        !Pointer to SymmOp. of stabilizer
   !!----     integer,                       optional, intent(in    ) :: Ipr       !Printing unit for debug
   !!----
   !!----  Subroutine to get the appropriate constraints in the refinement codes of
   !!----  anisotropic atomic displacement(thermal) parameters.
   !!----  New algorithm based in the Wigner theorem.
   !!----  The matrix Bet = Sum { R Beta RT} displays the symmetry constraints to be
   !!----  applied to the anisotropic temperature factors. The sum runs over all rotational
   !!----  symmetry operators of the stabilizer of the particular atom position in the given
   !!----  space group.
   !!----  There are a total of 29 kind of relations that may appear in the Bet matrix:
   !!----
   !!----     1    A A A 0   0   0  -> m-3m, -43m, 432, m-3,23, 3[111].2[001]
   !!----     2    A A C 0   0   0  -> 4/mmm, -42m, 4mm, 422, 4/m, -4,4, 4[001]
   !!----     3    A B A 0   0   0  -> 4[010]
   !!----     4    A B B 0   0   0  -> 4[100]
   !!----     5    A A A D   D   D  -> -3m, 3m, 32, -3, 3   3[111]
   !!----     6    A A A D  -D  -D  -> 3[11-1]
   !!----     7    A A A D  -D   D  -> 3[1-11]
   !!----     8    A A A D   D  -D  -> 3[-111]
   !!----     9    A A C A/2 0   0  -> 6/mmm, -6m2, 6mm, 622, 6/m, 6,-6,-3m, 32,-3, 3:  h 3[001]
   !!----    10    A B C 0   0   0  -> mmm, mm2, 222  2[001] 2[100]
   !!----    11    A A C D   0   0  -> 2[001], 2[110]    w
   !!----    12    A B A 0   E   0  -> 2[010], 2[101]
   !!----    13    A B B 0   0   F  -> 2[100], 2[011]
   !!----    14    A B C B/2 0   0  -> 2[001], 2[100]    h
   !!----    15    A B C A/2 0   0  -> 2[001], 2[010]    h
   !!----    16    A B C D   0   0  -> 2/m, m, 2: 2[001] w
   !!----    17    A B C 0   E   0  -> 2[010]
   !!----    18    A B C 0   0   F  -> 2[100]
   !!----    19    A A C D   E  -E  -> 2[110]            w
   !!----    20    A A C D   E   E  -> 2[1-10]           w
   !!----    21    A B A D   E  -D  -> 2[101]
   !!----    22    A B A D   E   D  -> 2[10-1]
   !!----    23    A B B D  -D   F  -> 2[011]
   !!----    24    A B B D   D   F  -> 2[01-1]
   !!----    25    A B C B/2 F/2 F  -> 2[100]            h
   !!----    26    A B C A/2 0   F  -> 2[210]            h
   !!----    27    A B C B/2 E   0  -> 2[120]            h
   !!----    28    A B C A/2 E   E/2-> 2[010]            h
   !!----    29    A B C D   E   F  -> 1, -1
   !!----
   !!----   Updated: 14 April 2023
   !!----
   !!
   Module Subroutine Get_AtomBet_CTR(X, Betas, Spgr, Codini, Icodes, Multip, Ord, Ss, Ipr)
      !---- Arguments ----!
      real(kind=cp), dimension(3),             intent(in    ) :: X          !Atom position (fractional coordinates)
      real(kind=cp), dimension(6),             intent(in out) :: Betas      !Anisotropic temperature factors
      type(SpG_type),                          intent(in    ) :: Spgr       !Space Group
      integer,                                 intent(in out) :: Codini     !Last attributed parameter
      integer, dimension(6),                   intent(in out) :: Icodes     !codewords for betas only number
      real(kind=cp), dimension(6),             intent(in out) :: Multip     !Multipliers
      integer,                       optional, intent(in    ) :: Ord        !Order of the stabilizer
      integer, dimension(:),         optional, intent(in    ) :: Ss         !Pointer to SymmOp. of stabilizer
      integer,                       optional, intent(in    ) :: Ipr        !Printing unit for debug

      !---- Local variables ----!
      real(kind=cp),     parameter      :: EPSS=0.01_cp

      character (len=1), dimension(6)   :: cdb
      integer                           :: i,j,order
      integer,           dimension(48)  :: ss_ptr
      integer,           dimension(6)   :: codd
      integer,           dimension(3,3) :: Rsym
      real(kind=cp),     dimension(3,3) :: bet,bett,Rs
      real(kind=cp),     dimension(6)   :: cod
      real(kind=cp),     dimension(3,48):: atr

      !> Init
      cod=real(icodes)

      do j=1,6
         if (cod(j) < 1.0 .and. abs(multip(j)) > EPSS)  then
            codini=codini+1
            cod(j) = real(codini)
         end if
      end do

      if (present(ord) .and. present(ss)) then
         order=ord
         ss_ptr(1:order) = ss(1:ord)
      else
         call get_stabilizer(x,Spgr,order,ss_ptr,atr)
      end if

      bet=reshape([17.0, 7.0,3.0,  &
                    7.0,13.0,5.0,  &
                    3.0, 5.0,11.0],[3,3])
      bett=bet
      if (order > 1 ) then
         do j=2,order
            !Rsym=Spgr%SymOp(ss_ptr(j))%Rot
            Rs=Spgr%Op(ss_ptr(j))%Mat(1:3,1:3)
            bett=bett+ matmul(Rs,matmul(bet,transpose(Rs)))
         end do
      end if
      Rsym=nint(1000.0*bett)
      codd=[Rsym(1,1),Rsym(2,2),Rsym(3,3),Rsym(1,2),Rsym(1,3),Rsym(2,3)]
      cdb=['a','b','c','d','e','f']
      multip=1.0_cp

      !> Search systematically all the possible constraints
      if (codd(1) == codd(2) .and. codd(1) == codd(3)) then ! a a a
         if (codd(4) == codd(5) .and. codd(4) == codd(6) ) then ! a a a d d d
            if (codd(4) == 0) then
               cdb=['a','a','a','0','0','0']     ! 1 A A A 0   0   0
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp, 0.0_cp]
               betas(4:6)=0.0_cp
               betas(2:3)=betas(1)
               cod(2:3)=cod(1); cod(4:6)=0.0_cp

            else
               cdb=['a','a','a','d','d','d']     ! 5 A A A D   D   D
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp]
               betas(5:6)=betas(4)
               betas(2:3)=betas(1)
               cod(2:3)=cod(1); cod(5:6)=cod(4)
            end if

         else if (codd(4) == -codd(5) .and. codd(4) == -codd(6) ) then !a a a d -d -d
            cdb=['a','a','a','d','d','d']       ! 6 A A A D  -D  -D
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, -1.0_cp, -1.0_cp]
            betas(5:6)=-betas(4)
            betas(2:3)=betas(1)
            cod(2:3)=cod(1); cod(5:6)=cod(4)

         else if (codd(4) == -codd(5) .and. codd(4) ==  codd(6) ) then !a a a d -d  d
            cdb=['a','a','a','d','d','d']       ! 7 A A A D  -D   D
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, -1.0_cp, 1.0_cp]
            betas(5)=-betas(4); betas(6)=betas(4)
            betas(2:3)=betas(1)
            cod(2:3)=cod(1); cod(5:6)= cod(4)

         else if (codd(4) ==  codd(5) .and. codd(4) == -codd(6) ) then !a a a d  d -d
            cdb=['a','a','a','d','d','d']       ! 8 A A A D   D  -D
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, -1.0_cp]
            betas(6)=-betas(4); betas(5)=betas(4)
            betas(2:3)=betas(1)
            cod(2:3)=cod(1); cod(5:6)= cod(4)
         end if

      else if (codd(1) == codd(2)) then ! a a c
         if (codd(4) == codd(5) .and. codd(4) == codd(6) .and. codd(4) == 0) then ! a a c 0 0 0
            cdb=['a','a','c','0','0','0']     ! 2 A A C 0   0   0
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp, 0.0_cp]
            betas(4:6)=0.0
            betas(2)=betas(1)
            cod(2)=cod(1); cod(4:6)= 0.0_cp

         else if (codd(5) == codd(6) .and. codd(5) == 0) then ! a a c x 0 0
            if (codd(4) == codd(1)/2) then
               cdb=['a','a','c','a','0','0']     ! 9 A A C A/2 0   0
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.5_cp, 0.0_cp, 0.0_cp]
               betas(5:6)=0.0_cp; betas(4)=betas(1)*0.5_cp
               betas(2)=betas(1)
               cod(2)=cod(1); cod(4)= cod(1); cod(5:6)=0.0_cp

            else
               cdb=['a','a','c','d','0','0']     !11 A A C D   0   0
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp]
               betas(5:6)=0.0_cp
               betas(2)=betas(1)
               cod(2)=cod(1); cod(5:6)=0.0_cp
            end if

         else
            if (codd(5) == codd(6)) then  ! a a c d e e
               cdb=['a','a','c','d','e','e']     !20 A A C D   E   E
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp]
               betas(6)=betas(5)
               betas(2)=betas(1)
               cod(2)=cod(1); cod(6)=cod(5)

            else if (codd(5) == -codd(6)) then  ! a a c d e -e
               cdb=['a','a','c','d','e','e']     !19 A A C D   E  -E
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, -1.0_cp]
               betas(6)=-betas(5)
               betas(2)=betas(1)
               cod(2)=cod(1); cod(6)=cod(5)
            end if
         end if

      else if (codd(1) == codd(3)) then ! a b a
         if (codd(4) == codd(6)) then    ! a b a d x d
            if (codd(4) == 0) then  ! a b a 0 x 0
               if (codd(5) == 0) then ! a b a 0 0 0
                  cdb=['a','b','a','0','0','0']     ! 3 A B A 0   0   0
                  multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp, 0.0_cp]
                  betas(4:6)=0.0_cp
                  betas(3)=betas(1)
                  cod(3)=cod(1); cod(4:6)=0.0_cp

               else                  ! a b a 0 e 0
                  cdb=['a','b','a','0','e','0']     !12 A B A 0   E   0
                  multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 1.0_cp, 0.0_cp]
                  betas(4)=0.0_cp;  betas(6)=0.0_cp
                  betas(3)=betas(1)
                  cod(3)=cod(1); cod(4)=0.0_cp;  cod(6)=0.0_cp
               end if

            else  !! a b a d e d
               cdb=['a','b','a','d','e','d']       !22 A B A D   E   D
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp]
               betas(6)=betas(4)
               betas(3)=betas(1)
               cod(3)=cod(1); cod(6)=cod(4)
            end if

         else if (codd(4) == -codd(6)) then ! a b a d e -d
            cdb=['a','b','a','d','e','d']         !21 A B A D   E  -D
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, -1.0_cp]
            betas(6)=-betas(4)
            betas(3)=betas(1)
            cod(3)=cod(1); cod(6)=cod(4)
         end if

      else if (codd(2) == codd(3)) then ! a b b
         if (codd(4) == codd(5)) then    ! a b b d d x
            if (codd(4) == 0) then  ! a b b 0 0 x
               if (codd(6) == 0) then ! a b b 0 0 0
                  cdb=['a','b','b','0','0','0']     ! 4 A B B 0   0   0
                  multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp, 0.0_cp]
                  betas(4:6)=0.0_cp
                  betas(3)=betas(2)
                  cod(3)=cod(2); cod(4:6)=0.0_cp

               else                  ! a b b 0 0 f
                  cdb=['a','b','b','0','0','f']     !13 A B B 0   0   F
                  multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp, 1.0_cp]
                  betas(4:5)=0.0_cp
                  betas(3)=betas(2)
                  cod(3)=cod(2); cod(4:5)=0.0_cp
               end if

            else  !! a b b d d f
               cdb=['a','b','b','d','d','f']       !24 A B B D   D   F
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp]
               betas(5)=betas(4)
               betas(3)=betas(2)
               cod(3)=cod(2); cod(5)=cod(4)
            end if

         else if (codd(4) == -codd(5)) then ! a b b d -d e
            cdb=['a','b','b','d','d','f']         !23 A B B D  -D   F
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, -1.0_cp, 1.0_cp]
            betas(5)=-betas(4)
            betas(3)=betas(2)
            cod(3)=cod(2); cod(5)=cod(4)
         end if

      else !Now a /= b /= c
         if (codd(4) == codd(5) .and. codd(4) == 0) then ! a b c 0 0 x
            if (codd(6) == 0) then ! a b c 0 0 0
               cdb=['a','b','c','0','0','0']          !10 A B C 0   0   0
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp, 0.0_cp]
               betas(4:6)=0.0_cp
               cod(4:6)=0.0_cp

            else
               cdb=['a','b','c','0','0','f']          !18 A B C 0   0   F
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp, 1.0_cp]
               betas(4:5)=0.0_cp
               cod(4:5)=0.0_cp
            end  if

         else if (codd(5) == codd(6) .and. codd(5) == 0) then  ! a b c x 0 0
            if (codd(4) == codd(1)/2) then ! a b c a/2 0 0
               cdb=['a','b','c','a','0','0']          !15 A B C A/2 0   0
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.5_cp, 0.0_cp, 0.0_cp]
               betas(5:6)=0.0_cp; betas(4)=betas(1)*0.5_cp
               cod(4)=cod(1); cod(5:6)=0.0_cp

            else if(codd(4) == codd(2)/2) then    !a b c b/2 0 0
               cdb=['a','b','c','b','0','0']          !14 A B C B/2 0   0
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.5_cp, 0.0_cp, 0.0_cp]
               betas(5:6)=0.0_cp; betas(4)=betas(2)*0.5_cp
               cod(4)=cod(2); cod(5:6)=0.0_cp

            else
               cdb=['a','b','c','d','0','0']          !16 A B C D   0   0
               multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 0.0_cp]
               betas(5:6)=0.0_cp
               cod(5:6)=0.0_cp
            end if

         else if (codd(4) == codd(6) .and. codd(4) == 0) then !a b c 0 e 0
            cdb=['a','b','c','0','e','0']            !17 A B C 0   E   0
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.0_cp, 1.0_cp, 0.0_cp]
            betas(4)=0.0_cp; betas(6)=0.0_cp
            cod(4)=0.0_cp; cod(6)=0.0_cp

         else if (codd(4) == codd(1)/2 .and. codd(5) == 0) then !a b c a/2 0 f
            cdb=['a','b','c','a','0','f']            !26 A B C A/2 0   F
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.5_cp, 0.0_cp, 1.0_cp]
            betas(4)=betas(1)*0.5_cp; betas(5)=0.0_cp
            cod(4)=cod(1); cod(5)=0.0_cp

         else if (codd(4) == codd(2)/2 .and. codd(6) == 0) then !a b c b/2 e 0
            cdb=['a','b','c','b','e','0']            !27 A B C B/2 E   0
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.5_cp, 1.0_cp, 0.0_cp]
            betas(4)=betas(2)*0.5_cp; betas(6)=0.0_cp
            cod(4)=cod(2); cod(6)=0.0_cp

         else if (codd(4) == codd(2)/2 .and. codd(5) == codd(6)/2) then !a b c b/2 f/2 f
            cdb=['a','b','c','b','f','f']            !25 A B C B/2 F/2 F
            multip=[1.0,1.0,1.0,0.5,0.5,1.0]
            betas(4)=betas(2)*0.5; betas(5)=betas(6)*0.5
            cod(4)=cod(2); cod(5)=cod(6)

         else if(codd(4) == codd(1)/2 .and. codd(6) == codd(5)/2) then !a b c a/2 e e/2
            cdb=['a','b','c','a','e','e']            !28 A B C A/2 E   E/2
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 0.5_cp, 1.0_cp, 0.5_cp]
            betas(4)=betas(1)*0.5_cp; betas(6)=betas(5)*0.5_cp
            cod(4)=cod(1); cod(6)=cod(5)

         else
            cdb=['a','b','c','d','e','f']            !29 A B C D   E   F
            multip=[1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp, 1.0_cp]
         end if
      end if

      do j=1,6
         if (multip(j) < EPSS .or. cdb(j) == "0" ) then
            icodes(j) = 0

         else
            icodes(j) = nint(cod(j))
         end if
      end do

      if (present(Ipr)) then
         write(Ipr,'(a,6i5)')           '     Codes on Betas       :  ',Icodes
         write(Ipr,'(a,6(a,1x),6f7.3)') '     Codes and multipliers:  ',cdb,multip
         write(Ipr,'(a)')               '     Beta_TOT matrix:  '
         do i=1,3
            write(Ipr,'(a,3f12.4)')       '                      ',bett(i,:)
         end do
      end if
   End Subroutine Get_AtomBet_CTR

   !!----
   !!----  Module Subroutine Get_AtomPos_CTR(X,Spgr,Codini,Icodes,Multip,Ord,Ss,Att,Ipr)
   !!----     real(kind=cp), dimension(3),       intent(in    ) :: x      !Atom position (fractional coordinates)
   !!----     type(SPG_Type),                    intent(in    ) :: Spgr   !Space Group
   !!----     Integer,                           intent(in out) :: codini !Last attributed parameter
   !!----     integer,       dimension(3),       intent(in out) :: Icodes
   !!----     real(kind=cp), dimension(3),       intent(in out) :: Multip
   !!----     integer,                 optional, intent(in    ) :: Ord
   !!----     integer, dimension(:),   optional, intent(in    ) :: Ss
   !!----     integer, dimension(:,:), optional, intent(in    ) :: Atr
   !!----     integer,                 optional, intent(in    ) :: Ipr
   !!----
   !!----     Subroutine to get the appropriate constraints in the refinement codes of
   !!----     atoms positions. The algorithm is based in an analysis of the symbol generated
   !!----     for the symmetry elements of the operators belonging to the stabilizer of the
   !!----     atom position. This routine operates with splitted codes in the sense of
   !!----     FullProf rules
   !!----
   !!----     Updated: April - 2023
   !!
   Module Subroutine Get_AtomPos_CTR(X, Spgr, Codini, ICodes, Multip, Ord, Ss, Att,Ipr)
      !---- Arguments ----!
      real(kind=cp), dimension(3),            intent(in)     :: X
      type(SpG_type),                         intent(in)     :: Spgr
      integer,                                intent(in out) :: Codini
      integer,       dimension(3),            intent(in out) :: ICodes
      real(kind=cp), dimension(3),            intent(in out) :: Multip
      integer,                       optional,intent(in)     :: Ord
      integer, dimension(:),         optional,intent(in)     :: Ss
      real(kind=cp), dimension(:,:), optional,intent(in)     :: Att
      integer,                       optional,intent(in)     :: Ipr

      !---- Local variables ----!
      real(kind=cp),     parameter     :: EPSS=0.001_cp

      integer                          :: i,j,k,order,L,L1,L2,ipar,j1
      integer,          dimension(3,3) :: RSym
      integer,          dimension(48)  :: ss_ptr
      real(kind=cp),    dimension(3,48):: atr
      real(kind=cp),    dimension(3)   :: tr

      character(len=80)                :: symbol,tsymbol,sym_symb
      character(len=10), dimension(3)  :: nsymb
      character(len=3),  dimension(3)  :: ssymb

      !type(Symm_Oper_Type)             :: Op

      if (present(ord) .and. present(ss) .and. present(att)) then
         order=ord
         ss_ptr(1:order) = ss(1:ord)
         atr(:,1:order)  = att(:,1:ord)
      else
         call get_stabilizer(x,Spgr,order,ss_ptr,atr)
      end if

      !> If codes were not assigned with explicit number
      !> attribute numbers bigger than initial Codini
      do j=1,3
         if (Icodes(j) < 1  .and. abs(multip(j)) > EPSS)  then
            codini = codini+1
            Icodes(j) = codini
         end if
      end do

      ssymb=["  x","  y","  z"]

      if (present(Ipr)) write(unit=Ipr,fmt='(/a,3f10.5)')  ' => Atom Position:',x

      if (order > 1 ) then  !A constraint in position must exist
         if (present(Ipr)) write(unit=Ipr,fmt='(a)')   ' => List of symmetry element of the stabilizer without identity:'
         do k=2,order
            symbol=" "
            !Rsym=Spgr%SymOp(ss_ptr(k))%Rot
            !tr=Spgr%SymOp(ss_ptr(k))%tr + atr(:,k)
            Rsym=Spgr%Op(ss_ptr(k))%Mat(1:3,1:3)
            tr=Spgr%Op(ss_ptr(k))%Mat(1:3,4)
            tr=tr+atr(:,k)

            !call Get_SymSymb(Rsym,tr,Sym_Symb)
            !call symmetry_symbol(Sym_Symb,tsymbol)
            Sym_Symb=Get_Symb_from_OP(Rsym,tr)
            tsymbol=symmetry_symbol(Rsym,tr)

            i=index(tsymbol,";")
            if (i /= 0) then
               symbol=tsymbol(1:i-1)
               !call Read_Xsym(tsymbol(i+1:),1,Rsym,Tr,.false.)

               tr=Get_Vec_from_FracStr(tsymbol(i+1:))
               if (sum(abs(x-tr)) < EPSS) then
                  ssymb=["  0","  0","  0"]
                  if (present(Ipr)) then
                     write(unit=Ipr,fmt="(a,i2,a,t20,a,t55,a,t90,4a)") "     Operator ",k,": ", &
                     trim(Sym_Symb),trim(tsymbol),"  ssymb:" ,(ssymb(j)//"  ",j=1,3)
                  end if
                  cycle
               end if

            else
               symbol=tsymbol
            end if

            ipar=index(symbol,")")              !Translation element appears before position
            L =index(symbol(ipar+1:)," ")+ipar  !Position of the first blank after translation
            L1=index(symbol(ipar+1:),",")+ipar  !Position of the first comma after translation
            L2=index(symbol(L1+1:),",")+L1      !Position of the second comma
            if (L1 == 0) L1=1
            if (L2 == 0) L2=1
            if (L  == 0) L=1

            !> Construct a new symbol that estabish automatically the constraints
            nsymb = [symbol(L+1:L1-1),symbol(L1+1:L2-1),symbol(L2+1:)]
            do i=1,3
               do j=1,10  !Delete unwanted symbols (keep only x,y,z,2 and -
                  if (nsymb(i)(j:j) == " ") cycle
                  if (nsymb(i)(j:j) /= "x" .and. nsymb(i)(j:j) /= "y" .and. &
                      nsymb(i)(j:j) /= "z" .and. nsymb(i)(j:j) /= "-" .and. &
                      nsymb(i)(j:j) /= "2" ) nsymb(i)(j:j)=" "
               end do
               if (len_trim(nsymb(i))  == 0 .or. (index(nsymb(i),"x") == 0 .and. &
                   index(nsymb(i),"y") == 0 .and. index(nsymb(i),"z") == 0  ) ) then
                  ssymb(i)="  0"
                  cycle
               end if

               !> Now remove 2s on the right of x,y, or z
               j1=index(nsymb(i),"2")
               if ( j1 /= 0) then
                  if (len_trim(nsymb(i)) == j1) nsymb(i)=nsymb(i)(1:j1-1)
               end if

               !> Now remove -s on the right of x,y, or z
               j1=index(nsymb(i),"-")
               if ( j1 /= 0) then
                  if (len_trim(nsymb(i)) == j1) nsymb(i)=nsymb(i)(1:j1-1)
               end if
               nsymb(i)= adjustl(nsymb(i))
            end do

            if (ssymb(1) /= "  0" .and. ssymb(1) /= "  a") then
               ssymb(1)= nsymb(1)
               ssymb(1)= adjustr(ssymb(1))
            end if

            if (ssymb(2) /= "  0" .and. ssymb(2) /= "  a" .and. ssymb(2) /= "  b" .and. &
               ssymb(2) /= " -a" .and. ssymb(2) /= " 2a"   ) then
               ssymb(2) = nsymb(2)
               ssymb(2) = adjustr(ssymb(2))
            end if

            if (ssymb(3) /= "  0" .and. ssymb(3) /= "  a" .and. ssymb(3) /= "  b" .and. &
               ssymb(3) /= "  c" .and. ssymb(3) /= " 2a" .and. ssymb(3) /= " 2b" .and. &
               ssymb(3) /= " -a" .and. ssymb(3) /= " -b") then
               ssymb(3) = nsymb(3)
               ssymb(3) = adjustr(ssymb(3))
            end if

            do i=1,3
               if (ssymb(i)(3:3) == "x")  ssymb(i)(3:3) = "a"
            end do
            do i=1,3
               if (ssymb(i)(3:3) == "y")  ssymb(i)(3:3) = "b"
            end do
            do i=1,3
               if (ssymb(i)(3:3) == "z")  ssymb(i)(3:3) = "c"
            end do
            if (present(Ipr)) then
               write(unit=Ipr,fmt="(a,i2,a,t20,a,t55,a,t90,4a)") "     Operator ",k,": ", &
               trim(Sym_Symb),trim(tsymbol),"  Ssymb:" ,(ssymb(j)//"  ",j=1,3)
            end if

         end do !do k=1,order  over operators of the stabilizer

      else
         ssymb=["  a","  b","  c"]

      end if  !order > 1

      do i=1,3                  !Fixing codes
         if (ssymb(i)=="  0") then
            Icodes(i)=0
            multip(i)=0.0
         end if
      end do

      if (index(ssymb(1),"a") /= 0) then
         do i=2,3  !Fixing codes
            if (index(ssymb(i),"-a") /= 0) then
               Icodes(i)=Icodes(1)
               multip(i)=-multip(1)

            else if (index(ssymb(i),"a") /= 0) then
               Icodes(i)=Icodes(1)
               multip(i)=multip(1)

               if (index(ssymb(i),"2") /= 0) then
                  multip(i)=2.0* multip(1)

               else if (index(ssymb(1),"2") /= 0) then
                  multip(i)=0.5* multip(1)
               end if
            end if
         end do

      else  !the x-coordinate is fixed, analyse y and z
         if (index(ssymb(2),"b") /= 0 .and. index(ssymb(3),"b") /= 0) then
            Icodes(3)=Icodes(2)
            if (ssymb(2) == ssymb(3)) then
               multip(3)= multip(2)

            else if (ssymb(3) == " -b" .and. ssymb(2) == "  b") then
               multip(3)= -multip(2)

            else if (ssymb(3) == "  b" .and. ssymb(2) == " -b") then
               multip(3)= -multip(2)
            end if
         end if
      end if

      do j=1,3
         if (abs(multip(j)) < EPSS) then
            Icodes(j) = 0
         end if
      end do

      if (present(Ipr)) then
         write(unit=Ipr,fmt="(a,3i5)")    "     Codes positions: ",Icodes
         write(unit=Ipr,fmt="(a,3f5.1)")  "     Multipliers    : ",multip
         write(unit=Ipr,fmt="(5a)")       "     Codes   string : ( ",(ssymb(j),j=1,3) ," )"
      end if
   End Subroutine Get_AtomPos_CTR

   !!
   !!----  Module Subroutine Get_moment_ctr(xnr,moment,Spg,codini,codes,ord,ss,att,Ipr,ctr_code)
   !!----     real(kind=cp), dimension(3),            intent(in    ) :: xnr      ! Atom position (fractional coordinates)
   !!----     real(kind=cp), dimension(3),            intent(in out) :: moment   ! Magnetic moment at position xnr
   !!----     class(SuperSpaceGroup_Type),            intent(in)     :: Spg      ! Super Space Group
   !!----     Integer,                                intent(in out) :: codini   ! Number of the Last attributed parameter
   !!----     real(kind=cp), dimension(3),            intent(in out) :: codes    ! codewords for magnetic moment
   !!----     real(kind=cp), dimension(3),   optional,intent(in)     :: side
   !!----     integer,                       optional,intent(in)     :: ord      ! Order of stabilizer
   !!----     integer, dimension(:),         optional,intent(in)     :: ss       ! Pointer to operators
   !!----     real(kind=cp), dimension(:,:), optional,intent(in)     :: att      ! Translations of stabilizer operators
   !!----     integer,                       optional,intent(in)     :: Ipr      ! Logical unit for writing
   !!----     character(len=*),              optional,intent(out)    :: ctr_code ! Symmetry code
   !!----
   !!----  Subroutine to get the appropriate constraints in the refinement codes of
   !!----  magnetic moment parameters.
   !!----  Algorithm based in the Wigner theorem.
   !!----  The vector Mom = Sum { R Moment} displays the symmetry constraints to be
   !!----  applied to the magnetic moments. The sum runs over all magnetic
   !!----  matrices of the stabilizer of the particular atom position in the given
   !!----  space group.
   !!----
   !!----   Updated: March 2020
   !!----
   !!
   Module Subroutine Get_Moment_CTR(xnr,moment,Spg,codini,Icodes,Multip,side,ord,ss,att,Ipr,ctr_code)
      real(kind=cp), dimension(3),            intent(in)     :: xnr
      real(kind=cp), dimension(:),            intent(in out) :: moment
      class(SpG_type),                        intent(in)     :: Spg
      Integer,                                intent(in out) :: codini
      integer,       dimension(3),            intent(in out) :: Icodes
      real(kind=cp), dimension(3),            intent(in out) :: Multip
      real(kind=cp), dimension(3),   optional,intent(in)     :: side
      integer,                       optional,intent(in)     :: ord
      integer, dimension(:),         optional,intent(in)     :: ss
      real(kind=cp), dimension(:,:), optional,intent(in)     :: att
      integer,                       optional,intent(in)     :: Ipr
      character(len=*),              optional,intent(out)    :: ctr_code

      ! Local variables
      character(len=1),  dimension(3)   :: codd
      character(len=15), dimension(3)   :: St_Cod
      character(len=:), allocatable     :: mag
      integer                           :: i,j,order,n,ig,iss,npos
      real(kind=cp)                     :: suma
      integer,           dimension(48)  :: ss_ptr
      real(kind=cp),     dimension(3,48):: atr
      real(kind=cp),     dimension(3)   :: cod,multi,codes
      real(kind=cp),     dimension(3)   :: x, Rsym
      real(kind=cp),     dimension(3,3) :: magm,mg  !g, magm= delta * det(g) * g
      real(kind=dp),     dimension(3,3) :: sCtr
      real(kind=cp),     dimension(3)   :: momentL,TotMom
      character(len=80)                 :: Symb
      integer,           dimension(3,3) :: s
      real(kind=cp),     dimension(3)   :: t


      !Test if all codes are given ... in such a case the user constraints
      !are prevalent

      suma=0.0_cp
      if(present(ctr_code)) ctr_code="(0,0,0)"
      n=3 !Real moments -> three components
      do j=1,3
         suma=suma+abs(Icodes(j))+abs(multip(j))
      end do

      if(suma < epss ) return  !No refinement is required
      if(present(Ipr)) then
        write(Ipr,"(/,a)")         " => Calculation of symmetry constraints for magnetic moments "
      end if
      x=xnr
      !where(x < 0.0) x=x+1.0
      !where(x > 1.0) x=x-1.0

      if(present(ord) .and. present(ss) .and. present(att)) then
        order=ord
        ss_ptr(1:order) = ss(1:ord)
        atr(:,1:order)  = att(:,1:ord)
      else
        call get_stabilizer(x,SpG,order,ss_ptr,atr)
        if(present(ipr)) Write(unit=ipr,fmt="(a,i3)") " => Stabilizer without identity, order:",order
      end if

      momentL=moment
      !if(present(side)) momentL=momentL/side
      sCtr=0.0_cp
      if(order > 1) then
        do ig=1,order
          j=ss_ptr(ig)
          s=Spg%Op(j)%Mat(1:3,1:3)
          magm(:,:) = real(s)*Spg%Op(j)%dt*Spg%Op(j)%time_inv
          if(present(side)) then
            do i=1,3
              mg(i,:)= magm(i,:)/side
            end do
          end if
          sCtr=sCtr+mg   !magm !Adding constraint matrices for each operator of stabilizer
          !do i=1,3
          !  write(unit=*,fmt="(2(3f14.4,a))") mg(i,:),"  ->  ",sCtr(i,:)
          !end do
          if(present(ipr)) then
            t=Spg%Op(j)%Mat(1:3,4)
            Symb=Symmetry_Symbol(s,t)
            mag=Set_Symb_From_Mat(magm,["u","v","w"])
            if(Spg%Op(j)%time_inv < 0) then
              npos=index(Symb," ")
              Symb=Symb(1:npos-1)//"' "//Symb(npos+1:)
            end if
            Rsym=matmul(magm,momentL)
            write(unit=ipr,fmt='(a,i3,a,tr2,a40,tr2,3f12.4,tr4,a)') '     Operator ',j,": ",trim(Spg%Symb_Op(j))//"  MagMat: "//trim(mag), Rsym,trim(Symb)
          end if
        end do  !ig operators
        sCtr=sCtr/order
        suma=sum(abs(sCtr))
        !write(*,"(a,f10.4,a,i3)") " suma:",suma, "Mag_Type:", spg%mag_type
        if(suma < epss .or. spg%mag_type == 2) then !This corresponds to a grey point group
           moment=0.0_cp
           codes=0.0_cp
           if(present(Ipr)) then
             write(Ipr,"(a)")         " Grey point group or symmetry paramagnetic site: the attached moment is zero "
             write(Ipr,"(a,24f14.6)") " Final codes: ",codes(1:n)
             write(Ipr,"(a,24f14.6)") " Constrained moment: ",moment
           end if
           return
        end if
        TotMom=matmul(sCtr,momentL)
        if(present(side)) then
             !TotMom=TotMom*side
             if(present(Ipr))  write(unit=Ipr,fmt="(2(a,3f14.4))") " SIDE:",side, " MOMENT_TOT:",TotMom
            do i=1,3
              if(present(Ipr))  write(unit=Ipr,fmt="(3f14.4)") sCtr(i,:)
            end do
        end if
        if(present(Ipr)) then
          call Get_Refinement_Codes(n,TotMom,sCtr,iss,multi,codd,momentL,Ipr)
        else
          call Get_Refinement_Codes(n,TotMom,sCtr,iss,multi,codd,momentL)
        end if
        cod=0.0
        do j=1,n
          if(codd(j) /= "0") then
            do i=1,iss
              if(codd(j) == cdd(i)) then
                cod(j)=codini+i
                exit
              end if
            end do
          end if
        end do
        moment=momentL
        if(present(side)) moment=momentL*side
        codes=0.0
        do j=1,n
          if(abs(multi(j)) > epss)  codes(j) = sign(1.0_cp, multi(j))*(abs(cod(j))*10.0_cp + abs(multi(j)) )
        end do
        codini=codini+iss
        if(present(Ipr)) then
          Write(unit=Ipr,fmt="(a,i4)")       " Number of free parameters  : ",iss
          write(unit=Ipr,fmt="(a,3f14.6)")   " Multipliers                : ",(multi(j), j=1,n)
          write(unit=Ipr,fmt="(28a)")        " String with free parameters: ( ",(codd(j)//", ",j=1,n-1),codd(n)//" )"
          write(unit=Ipr,fmt="(a,3i6)")      " Resulting integer codes    : ", nint(cod(1:n))
          write(unit=Ipr,fmt="(a,3f14.6)")   " Final codes (FullProf-like): ",codes(1:n)
          write(unit=Ipr,fmt="(a,3f14.6)")   " Constrained Moment         : ",moment
        end if

      else !No restrictions

        codd(1:n)=cdd(1:n)
        multi(1:n)=1.0_cp
        do j=1,n
          cod(j)=codini+j
          codes(j) = (abs(cod(j))*10.0_cp + abs(multi(j)))
        end do
        codini=codini+n
        if(present(Ipr)) then
          write(unit=Ipr,fmt="(a)")         " General position, no constraints in moment "
          write(unit=Ipr,fmt="(28a)")       " String with free parameters: ( ",(codd(j)//", ",j=1,n-1),codd(n)//" )"
          write(unit=Ipr,fmt="(a,24i6)")    " Resulting integer codes: ", nint(cod(1:n))
          write(unit=Ipr,fmt="(a,24f14.6)") " Final codes: ",codes(1:n)
          write(unit=Ipr,fmt="(a,24f14.6)") " Constrained moment: ",moment
        end if

      end if

      Icodes=nint(cod)
      multip=multi

      if(present(ctr_code)) then
          do j=1,3
            St_cod(j)=" "
            if(abs(multi(j)) < 0.00001) then
              St_cod(j) = "0"
            else
              if(multi(j) == 1.0_cp) then
                 St_cod(j)=codd(j)
              else if(multi(j) == -1.0_cp) then
                 St_cod(j)="-"//codd(j)
              else
                 write(unit=St_cod(j),fmt="(f10.5,a)")  multi(j),"*"//codd(j)
                 St_cod(j)=adjustl(St_cod(j))
              end if
            end if
          end do
         write(unit=ctr_code,fmt="(28a)") " ( ",(St_cod(j)//", ",j=1,n-1),St_cod(j)//" )"
         ctr_code=pack_string(ctr_code)
      end if
   End Subroutine Get_Moment_CTR

   !!
   !!----  Subroutine get_moment_ctr_Wigner(xnr,moment,Spgr,codini,Icodes,Multip,side,ord,ss,att,Ipr,ctr_code)
   !!----     real(kind=cp), dimension(3),            intent(in    ) :: xnr    !Atom position (fractional coordinates)
   !!----     real(kind=cp), dimension(3),            intent(in out) :: moment !Moment at position xnr
   !!----     class(SPG_type),                        intent(in    ) :: Spgr   !Magnetic Space Group
   !!----     Integer,                                intent(in out) :: codini !Last attributed parameter
   !!----     integer,       dimension(3),            intent(in out) :: Icodes !Number of the parameters
   !!----     real(kind=cp), dimension(3),            intent(in out) :: Multip !Multipliers
   !!----     real(kind=cp), dimension(3),   optional,intent(in)     :: side
   !!----     integer,                       optional,intent(in)     :: ord
   !!----     integer, dimension(:),         optional,intent(in)     :: ss
   !!----     real(kind=cp), dimension(:,:), optional,intent(in)     :: att
   !!----     integer,                       optional,intent(in)     :: Ipr
   !!----     character(len=*),              optional,intent(out)    :: ctr_code
   !!----
   !!----  Subroutine to get the appropriate constraints in the refinement codes of
   !!----  magnetic moment parameters.
   !!----  Algorithm based in the Wigner theorem.
   !!----  The vector Mom = Sum { R Moment} displays the symmetry constraints to be
   !!----  applied to the magnetic moments. The sum runs over all magnetic
   !!----  matrices of the stabilizer of the particular atom position in the given
   !!----  space group.
   !!----
   !!----   Updated: 16 April 2016
   !!----
   !!
   Module Subroutine get_moment_ctr_Wigner(xnr,moment,Spgr,codini,Icodes,Multip,side,ord,ss,att,Ipr,ctr_code)
      real(kind=cp), dimension(3),            intent(in)     :: xnr
      real(kind=cp), dimension(3),            intent(in out) :: moment
      Class(SPG_type),                        intent(in)     :: Spgr
      Integer,                                intent(in out) :: codini
      integer,       dimension(3),            intent(in out) :: Icodes
      real(kind=cp), dimension(3),            intent(in out) :: Multip
      real(kind=cp), dimension(3),   optional,intent(in)     :: side
      integer,                       optional,intent(in)     :: ord
      integer, dimension(:),         optional,intent(in)     :: ss
      real(kind=cp), dimension(:,:), optional,intent(in)     :: att
      integer,                       optional,intent(in)     :: Ipr
      character(len=*),              optional,intent(out)    :: ctr_code

      ! Local variables
      character (len=4), dimension(3)   :: cdw
      character (len=4)                 :: cditem
      integer                           :: j,order, ig, npos
      real(kind=cp)                     :: suma,dif
      integer,           dimension(48)  :: ss_ptr
      integer,           dimension(3)   :: codd,msym
      real(kind=cp),     dimension(3,3) :: Rs
      real(kind=cp),     dimension(3)   :: x,cod,multi,mom,mome,Rsym,codes
      real(kind=cp),     dimension(3,48):: atr
      character(len=:),  allocatable    :: Symb,mag
      integer,           dimension(3,3) :: s
      real(kind=cp),     dimension(3)   :: t
      real(kind=cp),     parameter      :: epss=0.01_cp

      suma=0.0
      do j=1,3
         suma=suma+abs(Icodes(j))+abs(multip(j))
         cod(j)=abs(Icodes(j))             !Input Parameter number with sign
         multi(j)=Multip(j)                !Input Multipliers
         if(cod(j) < 1.0 .and. abs(multi(j)) > epss)  then
              codini=codini+1
              cod(j) = real(codini)
         end if
      end do
      if(suma < epss) return  !No refinement is required

      x=modulo_lat(xnr)

      if(present(ord) .and. present(ss) .and. present(att)) then
        order=ord
        ss_ptr(1:order) = ss(1:ord)
        atr(:,1:order)  = att(:,1:ord)
      else
        call get_stabilizer(x,Spgr,order,ss_ptr,atr)
      end if

      mom=[17.0, 7.0,5.0]
      if(present(side)) mom=mom/side
      mome=mom
      if(present(ipr)) Write(unit=ipr,fmt="(a,i3)") " => Magnetic stabilizer without identity, order:",order
      if (order > 1 ) then
         do ig=2,order
            j=ss_ptr(ig)
            s=Spgr%Op(j)%Mat(1:3,1:3)
            Rs = real(s)*Spgr%Op(j)%dt*Spgr%Op(j)%time_inv
            Rsym=matmul(Rs,mom)
            mome=mome + Rsym
            if(present(ipr)) then
              t=Spgr%Op(j)%Mat(1:3,4)
              Symb=Symmetry_Symbol(s,t)
              mag=Set_Symb_From_Mat(Rs,["u","v","w"])
              if(Spgr%Op(j)%time_inv < 0) then
                npos=index(Symb," ")
                Symb=Symb(1:npos-1)//"' "//Symb(npos+1:)
              end if
              write(unit=ipr,fmt='(a,i3,a,tr2,a,tr2,3f12.4,tr4,a)') '     Operator ',ig,": ",trim(Spgr%Symb_Op(j))//"  MagMat: "//trim(mag), Rsym,trim(Symb)
            end if
         end do
         mome=mome/real(order)
         if(present(side)) mome=mome*side
      end if
      msym=nint(1000.0*mome)
      codd=msym
      cdw=['a','b','c']
      multi=1.0

      !Search systematically all the possible constraints

      if(codd(1) == codd(2) .and. codd(1) == codd(3)) then ! a a a
        cdw=['a','a','a']     ! 1 A A A
        multi=[1.0,1.0,1.0]
        moment(2:3)=moment(1)
        cod(2:3)=cod(1)
        if(codd(1) == 0) then !No magnetic moment allowed for this site
          cod=0
          moment=0.0
          multi=0.0
          cdw=['0','0','0']
        end if

      else if(codd(1) == codd(2)) then ! a a c
        cdw=['a','a','c']    ! 2  A A C
        multi=[1.0,1.0,1.0]
        moment(2)=moment(1)
        cod(2)=cod(1)
        if(codd(1) == 0) then ! 0 0 c
          cod(1:2)=0
          moment(1:2)=0.0
          multi(1:2)=0.0
          cdw=['0','0','c']
        else if(codd(3) == 0) then  ! a a 0
          cod(3)=0
          moment(3)=0.0
          multi(3)=0.0
          cdw=['a','a','0']
        else if(codd(3) == -codd(1)) then  ! a a -a
          cod(3)=cod(1)
          moment(3)=-moment(1)
          multi(3)=-1.0
          cdw=['a ','a ','-a']
        end if

      else if(codd(1) == codd(3)) then ! a b a
        cdw=['a','b','a']     ! 3  A B A
        multi=[1.0,1.0,1.0]
        moment(3)=moment(1)
        cod(3)=cod(1)
        if(codd(1) == 0) then !0 b 0
          cod(1)=0; cod(3)=0
          moment(1)=0.0; moment(3)=0.0
          multi(1)=0.0; multip(3)=0.0
          cdw=['0','b','0']
        else if(codd(2) == 0) then  ! a 0 a
          cod(2)=0
          moment(2)=0.0
          multi(2)=0.0
          cdw=['a','0','a']
        else if(codd(2) == -codd(1)) then  ! a -a a
          cod(2)=cod(1)
          moment(2)=-moment(1)
          multi(2)=-1.0
          cdw=['a ','-a','a ']
        end if

      else if(codd(2) == codd(3)) then ! a b b
        cdw=['a','b','b']     ! 4  A B B
        multi=[1.0,1.0,1.0]
        moment(3)=moment(2)
        cod(3)=cod(2)
        if(codd(2) == 0) then !a 0 0
          cod(2:3)=0
          moment(2:3)=0.0
          multi(2:3)=0.0
          cdw=['a','0','0']
        else if(codd(1) == 0) then  ! 0 b b
          cod(1)=0
          moment(1)=0.0
          multi(1)=0.0
          cdw=['0','b','b']
        else if(codd(1) == -codd(2)) then  ! -b b b
          cod(1)=cod(2)
          moment(1)=-moment(2)
          multi(1)=-1.0
          cdw=['-b','b ','b ']
        end if

      else if(codd(1) == -codd(2)) then ! a -a b
        cdw=['a ','-a','b ']
        multi=[1.0,-1.0,1.0]
        moment(2)=-moment(1)
        cod(2)=cod(1)
        if(codd(3) == 0) then ! a -a 0
          cod(3)=0
          moment(3)=0.0
          multi(3)=0.0
          cdw=['a ','-a','0 ']
        end if

      else if(codd(1) == -codd(3)) then ! a b -a
        cdw=['a ','b ','-a']
        multi=[1.0,1.0,-1.0]
        moment(3)=-moment(1)
        cod(3)=cod(1)
        if(codd(2) == 0) then ! a 0 -a
          cod(2)=0
          moment(2)=0.0
          multi(2)=0.0
          cdw=['a ','0 ','-a']
        end if

      else if(codd(2) == -codd(3)) then ! a b -b
        cdw=['a ','b ','-b']
        multi=[1.0,1.0,-1.0]
        moment(3)=-moment(2)
        cod(3)=cod(2)
        if(codd(1) == 0) then ! 0 b -b
          cod(1)=0
          moment(1)=0.0
          multi(1)=0.0
          cdw=['0 ','b ','-b']
        end if

      else !Now a /= b /= c

        if(codd(1) == 0) then  !0 b c
          cod(1)=0
          moment(1)=0.0
          multi(1)=0.0
          cdw=['0','b','c']
        end if
        if(codd(2) == 0) then  !a 0 c
          cod(2)=0
          moment(2)=0.0
          multi(2)=0.0
          cdw=['a','0','c']
        end if
        if(codd(3) == 0) then  !a b 0
          cod(3)=0
          moment(3)=0.0
          multi(3)=0.0
          cdw=['a','b','0']
        end if

        !Comparison a,b
        if(codd(1) /= 0 .and. codd(2)/=0) then
          suma=real(codd(1))/real(codd(2))
          if(abs(suma) < 1.0) then
            suma=1.0/suma
            order=codd(2)/codd(1)
            dif=abs(suma-real(order))
            if(dif < epss) then
              cod(2)=cod(1)
              multi(2)=suma
              moment(2)=suma*moment(1)
              write(unit=cditem,fmt="(i2,a)") order,"a"
              !cdw=['a',cditem,'c']  !incompatible with Lahey compiler
              cdw(1)='a'
              cdw(2)=cditem
              cdw(3)='c'
            end if
          else
            order=codd(1)/codd(2)
            dif=abs(suma-real(order))
            if(dif < epss) then
              cod(1)=cod(2)
              multi(1)=suma
              moment(1)=suma*moment(2)
              write(unit=cditem,fmt="(i2,a)") order,"b"
              !cdw=[cditem,'b','c']
              cdw(1)=cditem
              cdw(2)='b'
              cdw(3)='c'
            end if
           end if
        end if

        !Comparison a,c
        if(codd(1) /= 0 .and. codd(3)/=0) then
          suma=real(codd(1))/real(codd(3))
          if(abs(suma) < 1.0) then
            suma=1.0/suma
            order=codd(3)/codd(1)
            dif=abs(suma-real(order))
            if(dif < epss) then
              cod(3)=cod(1)
              multi(3)=suma
              moment(3)=suma*moment(1)
              write(unit=cditem,fmt="(i2,a)") order,"a"
              !cdw=['a','b',cditem]
              cdw(1)='a'
              cdw(2)='b'
              cdw(3)=cditem
            end if
          else
            order=codd(1)/codd(3)
            dif=abs(suma-real(order))
            if(dif < epss) then
              cod(1)=cod(3)
              multi(1)=suma
              moment(1)=suma*moment(3)
              write(unit=cditem,fmt="(i2,a)") order,"c"
              !cdw=[cditem,'b','c']
              cdw(1)=cditem
              cdw(2)='b'
              cdw(3)='c'
            end if
           end if
        end if

        !Comparison b,c
        if(codd(2) /= 0 .and. codd(3)/=0) then
          suma=real(codd(2))/real(codd(3))
          if(abs(suma) < 1.0) then
            suma=1.0/suma
            order=codd(3)/codd(2)
            dif=abs(suma-real(order))
            if(dif < epss) then
              cod(3)=cod(2)
              multi(3)=suma
              moment(3)=suma*moment(2)
              write(unit=cditem,fmt="(i2,a)") order,"b"
              !cdw=['a','b',cditem]
              cdw(1)='a'
              cdw(2)='b'
              cdw(3)=cditem
            end if
          else
            order=codd(2)/codd(3)
            dif=abs(suma-real(order))
            if(dif < epss) then
              cod(2)=cod(3)
              multi(2)=suma
              moment(2)=suma*moment(3)
              write(unit=cditem,fmt="(i2,a)") order,"c"
              !cdw=['a',cditem,'c']
              cdw(1)='a'
              cdw(2)=cditem
              cdw(3)='c'
            end if
           end if
        end if

      end if
      codini=maxval(cod)
      do j=1,3
        if(abs(multi(j)) < epss .or. cdw(j) == '0' ) then
          codes(j) = 0.0_cp
        else if(multi(j) < 0) then
          codes(j) = sign(1.0_cp, multi(j))*(abs(cod(j))*10.0_cp + abs(multi(j)) )
          multip=multi
        else
          codes(j) = sign(1.0_cp, multip(j))*(abs(cod(j))*10.0_cp + abs(multip(j)) )
        end if
      end do

      icodes=nint(cod)
      multip=multi

      if(present(Ipr)) then
        write(Ipr,'(a,3f10.4)')        '     Codes on Moments     : ',codes
        Write(Ipr,'(a,3(a,1x),6f7.3)') '     Codes and multipliers: ',cdw,multip
        Write(Ipr,'(a,3f12.4)')        '     Moment_TOT vector    : ',mome
      end if
      if(present(ctr_code)) then
         write(unit=ctr_code,fmt="(5a)") " ( ",(cdw(j)//", ",j=1,2),cdw(j)//" )"
         ctr_code=pack_string(ctr_code)
      end if

   End Subroutine get_moment_ctr_Wigner

   Subroutine Get_Refinement_Codes(n,vect_val,Ctr,iss,multi,codd,vect_out,Ipr)
     integer,                       intent(in)    :: n !dimension of the vector and the matrix
     real(kind=cp), dimension(:),   intent(in)    :: vect_val
     real(kind=dp), dimension(:,:), intent(in out):: Ctr
     integer,                       intent(out)   :: iss
     real(kind=cp), dimension(:),   intent(out)   :: multi
     character(len=*), dimension(:),intent(out)   :: codd
     real(kind=cp), dimension(:),   intent(out)   :: vect_out
     integer,       optional,       intent(in)    :: Ipr
     !--- Local variables ---!
     real(kind=cp), dimension(n)   :: val
     integer,       dimension(n)   :: pti
     real(kind=dp), dimension(n,n) :: zv
     real(kind=dp), dimension(n,n) :: inpmat
     integer                       :: i,j,k,kval,ip !,ier
     real(kind=dp)                 :: zmi
     real(kind=dp), dimension(n)   :: Wr, Wi
     logical,       dimension(n)   :: done

     !Diagonalize the matrix and pickup the lambda=1 eigenvalues
     !The corresponding eigenvector contains the constraints of all moment components
     !Calling the general diagonalization subroutine from EisPack
     inpmat=Ctr(1:n,1:n)
     call Diagonalize_RGen(n,Ctr,wr,wi,.true.,zv)
     iss=0
     pti=0
     kval=0
     do i=1,n
       if(abs(wr(i)-1.0_dp) < epss .and. abs(wi(i)) < epss) then
         iss=iss+1   !Number of eigenvalues = 1 => number of free parameters
         pti(iss)=i  !This points to the eigenvectors with eigenvalue equal to 1.
         zmi=1.0e6   !normalize the eigenvectors so that the minimum (non-zero value) is 1.
         j=1
         do k=1,n
           if(abs(zv(k,i)) < epss) cycle
           if(abs(zv(k,i)) < zmi) then
             zmi=abs(zv(k,i))
             kval=k  !This is the basis value
             j=nint(sign(1.0_dp,zv(k,i)))
           end if
         end do
         !zv(1:n,i)=j*zv(1:n,i)/zmi  !This provides directly the multipliers for a single lambda=1 eigenvalue
         zv(1:n,i)=zv(1:n,i)/zmi    !This provides directly the multipliers for a single lambda=1 eigenvalue
         val(iss)=vect_val(kval)    !This is the basis value to construct the new Moment
       end if
     end do
     if(present(Ipr)) then
        write(unit=Ipr,fmt="(a)")        "  Input matrix to be diagonalized (Sum of symmetry operator matrices)"
        do i=1,n
            write(unit=Ipr,fmt="(a,6F12.4)") "    ", inpmat(i,1:n)
        end do
        write(unit=Ipr,fmt="(a)")        "  Normalized Eigen Vectors for eigenvalues = 1 :"
        write(unit=Ipr,fmt="(a,6F12.4)") "  Input vect_val: ",vect_val
        j=0
        do i=1,n
            if(abs(wr(i)-1.0_dp) < epss .and. abs(wi(i)) < epss) then
               j=j+1
               write(unit=Ipr,fmt="(a,2i5,7F12.4)") "  i,j, zv(1:n,i), val(j): ",i,j, zv(1:n,i), val(j)
            end if
        end do
     end if
     codd="0"
     vect_out=0.0
     multi=0.0
     done=.false.
     where(abs(vect_val) < epss) done=.true.
     Select Case(iss)
       case(1)
         vect_out(1:n)=val(1)*zv(1:n,pti(1))
         where(abs(vect_out) > epss)  codd(:)=cdd(1)
         multi(1:n)=zv(1:n,pti(iss))
       !case(2)
       case(2:)
         ip=0
         do i=1,n
           if(.not. done(i)) then
             if(abs(vect_val(i)) > epss) then
               ip=ip+1
               codd(i)=cdd(ip)
               multi(i)= 1.0  !zv(i,pti(iss))     !1.0
               vect_out(i)=vect_val(i)
               done(i)=.true.
               do j=i+1,n
                 if(.not. done(j)) then
                   if(abs(vect_val(i)-vect_val(j)) < epss) then
                     codd(j)=cdd(ip)
                     multi(j)=1.0  !zv(j,pti(iss)) !
                     vect_out(j)=vect_val(i)
                     done(j)=.true.
                   else if(abs(vect_val(i)+vect_val(j)) < epss) then
                     codd(j)=cdd(ip)
                     multi(j)= -1.0 !-zv(j,pti(iss)) !-1.0
                     vect_out(j)=-vect_val(i)
                     done(j)=.true.
                   end if
                 end if
               end do
             end if
           end if
         end do
     End Select
     if(present(Ipr)) then
        write(unit=Ipr,fmt="(a,6F12.4)") "  Input  vect_val: ",vect_val
        write(unit=Ipr,fmt="(a,6F12.4)") "  Output vect_out: ",vect_out
     end if
   End Subroutine Get_Refinement_Codes

   !!
   !!----  Module Subroutine Get_TFourier_CTR(xnr,TFourier,codes,SpG,codini,mode,ord,ss,att,Ipr,ctr_code)
   !!----     real(kind=cp), dimension(3),            intent(in    ) :: xnr      ! Atom position (fractional coordinates)
   !!----     real(kind=cp), dimension(:,:),          intent(in out) :: TFourier ! Fourier coefficients at position xnr
   !!----     real(kind=cp), dimension(:,:),          intent(in out) :: codes    ! codewords for Fourier components
   !!----     class(SuperSpaceGroup_Type),            intent(in)     :: Spg      ! Super Space Group
   !!----     Integer,                                intent(in out) :: codini   ! Number of the Last attributed parameter
   !!----     character(len=*),                       intent(in)     :: Mode     ! "M" or "D" for modulation moments or displacements
   !!----     integer,                       optional,intent(in)     :: ord      ! Order of stabilizer
   !!----     integer, dimension(:),         optional,intent(in)     :: ss       ! Pointer to operators
   !!----     real(kind=cp), dimension(:,:), optional,intent(in)     :: att      ! Translations of stabilizer operators
   !!----     integer,                       optional,intent(in)     :: Ipr      ! Logical unit for writing
   !!----     character(len=*),              optional,intent(out)    :: ctr_code ! Symmetry code
   !!----
   !!----  Subroutine to get the appropriate constraints in the refinement codes of
   !!----  magnetic moment parameters.
   !!----  Algorithm based in the Wigner theorem.
   !!----  The vector Mom = Sum { R Moment} displays the symmetry constraints to be
   !!----  applied to the magnetic moments. The sum runs over all magnetic
   !!----  matrices of the stabilizer of the particular atom position in the given
   !!----  space group.
   !!----
   !!----   Updated: March 2020
   !!----
   !!
   Module Subroutine Get_TFourier_CTR(xnr,TFourier,codes,SpG,codini,mode,ord,ss,att,Ipr,ctr_code)
      real(kind=cp), dimension(3),            intent(in)     :: xnr
      real(kind=cp), dimension(:,:),          intent(in out) :: TFourier
      real(kind=cp), dimension(:,:),          intent(in out) :: codes
      class(SuperSpaceGroup_Type),            intent(in)     :: SpG
      Integer,                                intent(in out) :: codini
      character(len=*),                       intent(in)     :: mode
      integer,                       optional,intent(in)     :: ord
      integer, dimension(:),         optional,intent(in)     :: ss
      real(kind=cp), dimension(:,:), optional,intent(in)     :: att
      integer,                       optional,intent(in)     :: Ipr
      character(len=*),dimension(:), optional,intent(out)    :: ctr_code

      ! Local variables
      integer,          dimension(SpG%nk)     :: brack_m  ! hs=(H,[m])
      real(kind=cp),    dimension(spg%d-1)    :: ts  ! ts=(t,tI)
      integer,          dimension(SpG%nk)     :: mE  ![m].E
      integer,          dimension(SpG%nk,3)   :: Mx  !M
      integer,          dimension(3,3)        :: g, magm   !g, magm= delta * det(g) * g
      integer,      dimension(SpG%nk,SpG%nk)  :: ep  !E
      character(len=1),  dimension(26)        :: codd
      character(len=15), dimension(26)        :: st_cod
      integer                                 :: i,j,order,iq,ir,d,n,k,iqt,ig,ip,iss,nq
      real(kind=cp)                           :: suma,alpha
      integer,           dimension(48)        :: ss_ptr
      real(kind=cp),     dimension(3,48)      :: atr
      real(kind=cp),     dimension(6*SpG%nq)  :: cod,multi !,val
      real(kind=cp),     dimension(3)         :: x
      real(kind=dp),     dimension(6,6)       :: subm
      real(kind=dp),  dimension(6*SpG%nq,6*SpG%nq) :: Ctr,sCtr
      real(kind=cp),     parameter                 :: epss=0.001_cp
      real(kind=cp), dimension(6*SpG%nq)           ::TFourL,sTF


      !Test if all codes are given ... in such a case the user constraints
      !are prevalent

      d=SpG%d-1
      suma=0.0
      !iq=0
      n=0
      nq=size(TFourier,dim=2)
      do k=1,nq
        do j=1,6
           n=n+1
           suma=suma+abs(codes(j,k))
        end do
      end do

      if(suma < epss ) return  !No refinement is required

      x=xnr
      where(x < 0.0) x=x+1.0
      where(x >= 1.0) x=x-1.0

      if(present(ord) .and. present(ss) .and. present(att)) then
        order=ord
        ss_ptr(1:order) = ss(1:ord)
        atr(:,1:order)  = att(:,1:ord)
      else
        call get_stabilizer(x,SpG,order,ss_ptr,atr)
        if(present(ipr)) Write(unit=ipr,fmt="(a,i3)") " => Superspace stabilizer without identity, order:",order
      end if

      do ip=1,nq
        i=(ip-1)*6
        TFourL(i+1:i+6)=TFourier(:,ip)
      end do

      sCtr=0.0_cp
      sTF=0.0
      if(order > 1) then
        n=6*nq

        do ig=1,order
            ir=ss_ptr(ig)
              g(:,:) = SpG%Op(ir)%Mat(1:3,1:3)   !                          [  g    0   t  ]
               ts(:) = SpG%Op(ir)%Mat(1:d,d+1)   !   Superspace operator:  [  Mx   ep   tI  ]    !ts=(t,tI)
             Mx(:,:) = SpG%Op(ir)%Mat(4:d,1:3)   !                          [   0    0   1 ]
             Ep(:,:) = SpG%Op(ir)%Mat(4:d,4:d)
             magm(:,:) = g(:,:)
             if(mode(1:1) == "M")  magm(:,:) = magm(:,:)*SpG%Op(ir)%time_inv*SpG%Op(ir)%dt
           !Identify the q_coefficients of mM to select the proper Tfourier
           !Construction of the Cr-matrix
           Ctr=0.0
           do ip=1,nq
             brack_m(:)=SpG%q_coeff(:,ip) !nk-components vector
             mE=matmul(brack_m,Ep)   ![m].Ep  nk-components vector
             do iqt=1,nq
               iq=iqt
               if(equal_vector(mE,SpG%q_coeff(:,iqt))) then
                 iss=1
                 exit
               end if
               if(equal_vector(mE,-SpG%q_coeff(:,iqt))) then
                 iss=-1  ! iss=-1
                 exit
               end if
             end do !iq
             alpha=tpi*dot_product(brack_m,matmul(Mx,x)+ts(4:))
             subm(1:3,1:3)=cos(alpha)*magm
             subm(4:6,1:3)=sin(alpha)*magm
             subm(1:3,4:6)=-iss*sin(alpha)*magm
             subm(4:6,4:6)= iss*cos(alpha)*magm
             !Block  ip,iq
             i=(ip-1)*6; j=(iq-1)*6
             Ctr(i+1:i+6,j+1:j+6)=subm
             Tfourl(i+1:i+6)=matmul(subm,TFourier(:,iq))
             sTF(i+1:i+6)=sTF(i+1:i+6)+TfourL(i+1:i+6)
           end do !ip
           sCtr=sCtr+Ctr !Adding constraint matrices for each operator of stabilizer
           if(present(ipr)) then
             do ip=1,nq
                i=(ip-1)*6
                write(unit=ipr,fmt='(a,i2,a,t20,a,t55,a,6f14.4,4i3)') '     Operator ',ir,": ",trim(Spg%Symb_Op(ir))," Matrix SubM x TFourier & [m] : ",Tfourl(i+1:i+6),brack_m(:)
             end do
           end if
        end do  !ig operators
        sCtr=sCtr/order
        sTF=sTF/order
        if(present(ipr)) then
          do ip=1,nq
             i=(ip-1)*6
             write(unit=ipr,fmt='(a,6f14.4)')     '     Sum of TFour: ',sTf(i+1:i+6)
          end do
        end if
        call Get_Refinement_Codes(n,sTF,sCtr,iss,multi,codd,TFourL)  !Get_Refinement_Codes(n,vect_val,Ctr,iss,multi,codd,vect_out)
        cod=0.0
        do j=1,n
          if(codd(j) /= "0") then
            do i=1,iss
              if(codd(j) == cdd(i)) then
                cod(j)=codini+i
                exit
              end if
            end do
          end if
        end do
        do ip=1,nq
          i=(ip-1)*6
          TFourier(:,ip)=TFourL(i+1:i+6)
        end do

        codes=0.0; j=0
        do ip=1,nq
          do i=1,6
            j=j+1
            if(abs(multi(j)) > epss)  codes(i,ip) = sign(1.0_cp, multi(j))*(abs(cod(j))*10.0_cp + abs(multi(j)) )
          end do
        end do
        codini=codini+iss
        if(present(Ipr)) then
          Write(Ipr,"(a,i4)")      " Number of free parameters: ",iss
          Write(Ipr,"(a,24F14.6)") " Basic Values: ",TFourL(1:iss)
          write(Ipr,"(a,24f14.6)") " Multipliers: ",(multi(j), j=1,n)
          write(Ipr,"(28a)")       " String with free parameters: ( ",(codd(j)//", ",j=1,n-1),codd(n)//" )"
          write(Ipr,"(a,24i6)")    " Resulting integer codes: ", nint(cod(1:n))
          do ip=1,nq
            write(Ipr,"(a,i2, 6f14.6)") " Constrained Fourier Coefficients: ",ip,TFourier(:,ip)
            write(Ipr,"(a,tr2,6f14.6)") " Final codes: ",codes(:,ip)
          end do
        end if

      else !No restrictions

        codd(1:n)=cdd(1:n)
        multi(1:n)=1.0_cp
        j=0
        do ip=1,nq
          do i=1,6
            j=j+1
            cod(j)=codini+j
            codes(i,ip) = abs(cod(j))*10.0_cp + abs(multi(j))
          end do
        end do
        codini=codini+n
        if(present(Ipr)) then
          write(Ipr,"(a,24f10.6)") " General position, no constraints in Fourier Coefficients "
          write(Ipr,"(28a)")       " String with free parameters: ( ",(codd(j)//", ",j=1,n-1),codd(n)//" )"
          write(Ipr,"(a,24i6)")    " Resulting integer codes: ", nint(cod(1:n))
          do ip=1,nq
            write(Ipr,"(a,i2, 6f14.6)") " Constrained Fourier Coefficients: ",ip,TFourier(:,ip)
            write(Ipr,"(a,tr2,6f14.6)") " Final codes: ",codes(:,ip)
          end do
        end if

      end if

      if(present(ctr_code)) then
        St_cod="0"
        n=0
        do ip=1,nq
          do j=1,6
            n=n+1
            St_cod(n)=" "
            if(abs(multi(n)) < 0.00001) then
              St_cod(n) = " 0 "
            else
              if(multi(n) == 1.0_cp) then
                 St_cod(n)=codd(n)
              else if(multi(n) == -1.0_cp) then
                 St_cod(n)="-"//codd(n)
              else
                 write(unit=St_cod(n),fmt="(f10.5,a)")  multi(n),"*"//codd(n)
                 St_cod(n)=adjustl(St_cod(n))
              end if
            end if
          end do
        end do
        n=0
        do ip=1,nq
          write(unit=ctr_code(ip),fmt="(8a)") " ( ",(St_cod(n+j)//", ",j=1,5),St_cod(n+6)//" )"
          ctr_code(ip)=pack_string(ctr_code(ip))
          n=n+6
        end do
      end if

   End Subroutine Get_TFourier_CTR

End SubModule gS_Get_Orb_Stabilizer_Constr