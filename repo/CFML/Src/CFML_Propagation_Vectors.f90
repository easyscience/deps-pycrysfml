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
!!---- MODULE: CFML_Propagation_Vectors
!!----   INFO: Series of procedures handling operation with Propagation
!!----         vectors
!!----
!!---- HISTORY
!!----    Update: 06/03/2011
!!----
!!----
!!---- DEPENDENCIES
!!--++    Use CFML_GlobalDeps,                only: Cp
!!--++    Use CFML_Maths,                     only: Zbelong,eps=>epss
!!--++    Use CFML_gSpaceGroups,              only: SpG_Type, Set_SpaceGroup
!!----
!!---- VARIABLES
!!--++    EPS                      [Private]
!!----    GROUP_K_TYPE
!!----
!!---- PROCEDURES
!!----    Functions:
!!----       HK_EQUIV
!!----       K_EQUIV
!!----       K_EQUIV_MINUS_K
!!----
!!----    Subroutines:
!!----       K_STAR
!!----       SET_GK
!!----       WRITE_GROUP_K
!!----
!!
 Module CFML_Propagation_Vectors

    !---- Use Modules ----!
    Use CFML_GlobalDeps,                only: Cp
    Use CFML_Maths,                     only: Zbelong, Equal_Vector, Eps => epss
    Use CFML_Rational
    Use CFML_gSpaceGroups,              only: SpG_Type, Set_SpaceGroup

    !---- Variables ----!
    implicit none

    private

    !---- List of public functions ----!
    public :: Hk_Equiv, K_Equiv , K_Equiv_Minus_K

    !---- List of public subroutines ----!
    public :: K_Star, Write_Group_K, Set_Gk

    !---- Definitions ----!

    !!----
    !!---- TYPE :: GROUP_K_TYPE
    !!--..
    !!---- Type, Public :: Group_K_Type
    !!----    type(SpG_Type)                :: G0             !Initial space group
    !!----    integer                       :: ngk            !Number of elements of G_k
    !!----    logical                       :: k_equiv_minusk !true if k equiv -k
    !!----    logical                       :: minusk         !true if there is at least one operator transforming k into -k (even if there's no inversion centre)
    !!----    logical                       :: extended=.false. !true if the pointers to the extended little group have been set
    !!----    integer,      dimension(192)  :: p              !Pointer to operations of G0 that changes/fix k
    !!----                                                    !First indices: G_k, last indices: Stark
    !!----    integer, dimension(48,48)     :: co             !Pointers of symmetry operators of G0 constituting the
    !!----                                                    !coset representatives of a particular k-vector of the star
    !!                                                        !Only the nk x nk submatrix is used, the rest is zero.
    !!----    integer                       :: nk             !Number of star arms
    !!----    real(kind=cp),dimension(3,48) :: stark          !Star of the wave vector k
    !!----    real(kind=cp),dimension(3,48) :: Eqv_k          !List of wave vectors eqivalent to initial k. The total number of vectors is ngk
    !!---- End Type Group_K_Type
    !!----
    !!--<<
    !!----    The integer pointer p is used as follows:
    !!----    If we defined the object G as ->  type(Group_K_Type) :: G
    !!----    G%p(1:ngk) gives the numeral of the symmetry operators of G%G0
    !!----    belonging to G_k.
    !!----    G%p(193-nk:192) gives the numeral of the the symmetry operators of G%G0 that
    !!----    transform the initial k-vector to the other arms of the star.
    !!----    G%co(:,kk) gives also the numerals of the the symmetry operators of G%G0 that
    !!----    transform the initial k-vector to the arm kk of the star to the representative
    !!----    of the coset decomposition of G%G0 with respect to G_k.
    !!-->>
    !!----
    !!---- Update: February - 2005
    !!
    Type, Public :: Group_k_Type
       type(SpG_Type)                :: G0     !Here the space group is one of the 230 colorless groups
       integer                       :: ngk=0  !Number of elements of G_k
       logical                       :: k_equiv_minusk=.false. !true if k equiv -k
       logical                       :: minusk=.false.   !true if there is at least one operator transforming k into -k (even if there's no inversion centre)
       logical                       :: extended=.false. !true if the pointers to the extended little group have been set
       integer, dimension(192)       :: p=0        !Pointer to operations of G0 that changes/fix k. First indices: G_k, last indices: Stark
       integer, dimension(48,48)     :: co=0       !coset representatives of a particular k-vector of the star
       integer                       :: nk=0       !Number of star arms
       real(kind=cp),dimension(3,48) :: stark=0.0  !Star of the wave vector k
       real(kind=cp),dimension(3,48) :: Eqv_k=0.0  !List of wave vectors eqivalent to initial k. The total number of vectors is ngk
    End Type Group_k_Type

 Contains

    !-------------------!
    !---- Functions ----!
    !-------------------!

    !!----
    !!---- Logical Function  Hk_Equiv(H,K, Spacegk ,Friedel)
    !!----    real(kind=cp), dimension(3), intent(in) :: h
    !!----    real(kind=cp), dimension(3), intent(in) :: k
    !!----    Type (Group_k_Type),         intent(in) :: SpaceGk
    !!----    logical, optional,           intent(in) :: Friedel
    !!----
    !!----    Calculate if two real reflections are equivalent according to the little group
    !!----
    !!---- Update: February - 2005
    !!
    Function Hk_Equiv(H,K,Spacegk,Friedel) Result (Info)
       !---- Arguments ----!
       real(kind=cp), dimension(3), intent (in) :: h,k
       Type (Group_k_Type),         intent (in) :: SpaceGk
       logical, optional,           intent(in)  :: Friedel
       logical                                  :: info

       !---- Local Variables ----!
       integer                    :: i,j
       real(kind=cp), dimension(3)  :: hh

       info=.false.
       do i=1, SpaceGk%ngk
          j=SpaceGk%p(i)
          hh=matmul(h,real(SpaceGk%g0%Op(j)%Mat(1:3,1:3)))
          if (equal_vector(k,hh)) then
             info=.true.
             exit
          end if
          if (present(Friedel) .and. SpaceGk%g0%centred /= 2 .and. SpaceGk%k_equiv_minusk) then
             if (Friedel) then
                if (equal_vector(k,-hh)) then
                   info=.true.
                   exit
                end if
             end if
          end if
       end do

    End Function Hk_Equiv

    !!----
    !!---- Logical Function  K_Equiv(H,K,Latyp)  Result (Info)
    !!----    real(kind=cp), dimension(3),    intent (in) :: h      !  In ->
    !!----    real(kind=cp), dimension(3),    intent (in) :: k      !  In ->
    !!----    character (len=*),              intent (in) :: latyp  !  In ->
    !!----
    !!----    Calculate if two k-vectors are equivalent in the sense
    !!----    that "h" is equivalent to "k" if "h-k" belongs to the reciprocal
    !!----    lattice. Only lattice type is needed.
    !!----
    !!---- Update: February - 2005
    !!
    Function K_Equiv(H,K,Latyp) Result (Info)
       !---- Arguments ----!
       real(kind=cp), dimension(3),intent (in) :: h,k
       character (len=*),          intent (in) :: latyp
       logical                                 :: info

       !---- Local Variables ----!
       character(len=1)             :: latypc
       real(kind=cp), dimension(3)  :: vec
       integer, dimension(3)        :: ivec

       info=.false.
       vec=h-k
       if (.not. Zbelong(vec) ) return
       ivec=nint(vec)
       latypc=adjustl(latyp)
       select case (Latypc)
          case("P","p")
             info=.true.
          case("A","a")
             if (mod(ivec(2)+ivec(3),2) == 0) info=.true.
          case("B","b")
             if (mod(ivec(1)+ivec(3),2) == 0) info=.true.
          case("C","c")
             if (mod(ivec(1)+ivec(2),2) == 0) info=.true.
          case("I","i")
             if (mod(ivec(1)+ivec(2)+ivec(3),2) == 0) info=.true.
          case("R","r")
             if (mod(-ivec(1)+ivec(2)+ivec(3),3) == 0) info=.true.
          case("F","f")
             if (mod(ivec(2)+ivec(3),2) == 0              &
                 .and. mod(ivec(1)+ivec(3),2) == 0        &
                 .and. mod(ivec(1)+ivec(2),2) == 0 ) info=.true.
       end select

    End Function K_Equiv

    !!----
    !!---- Logical Function K_Equiv_Minus_K(Vec,Lat) Result(Equiv)
    !!----    real(kind=cp), dimension(3), intent(in) :: vec      !  In ->
    !!----    character (len=*),           intent(in) :: Lat      !  In ->
    !!----    logical                                 :: equiv
    !!----
    !!----    Determine whether a k-vector is equivalent to -k.
    !!----
    !!---- Update: February - 2005
    !!
    Function K_Equiv_Minus_K(Vec,Lat) result(equiv)
       !---- Argument ----!
       real(kind=cp), dimension(3), intent(in) :: vec
       character(len=*),            intent(in) :: Lat
       logical                                 :: equiv

       !---- Local variables ----!
       character(len=1)      :: latc
       logical               :: integralv
       integer, dimension(3) :: ivec

       integralv=.false.

       if ( Zbelong(2.0*vec) )  integralv=.true.
       equiv=.false.
       latc=adjustl(lat)

       if (integralv) then
          ivec = nint (2.0 * vec)
          select case (latc)
             case("P","p")
                equiv=.true.

             case("A","a")
                if (mod(ivec(2)+ivec(3),2) == 0) equiv=.true.

             case("B","b")
                if (mod(ivec(1)+ivec(3),2) == 0) equiv=.true.

             case("C","c")
                if (mod(ivec(1)+ivec(2),2) == 0) equiv=.true.

             case("I","i")
                if (mod(ivec(1)+ivec(2)+ivec(3),2) == 0) equiv=.true.

             case("R","r")
                if (mod(-ivec(1)+ivec(2)+ivec(3),3) == 0) equiv=.true.

             case("F","f")
                if (mod(ivec(2)+ivec(3),2) == 0        &
                   .and. mod(ivec(1)+ivec(3),2) == 0        &
                   .and. mod(ivec(1)+ivec(2),2) == 0 ) equiv=.true.

          end select
       end if

    End Function K_Equiv_Minus_K

    !---------------------!
    !---- Subroutines ----!
    !---------------------!

    !!----
    !!---- Subroutine K_Star(K,Spacegroup,Gk,ext)
    !!----    real(kind=cp),dimension(3), intent(in)  :: k           !  In ->
    !!----    type (SpG_Type),            intent(in)  :: SpaceGroup  !  In ->
    !!----    Type (Group_k_Type),        intent(out) :: Groupk      ! Out ->
    !!----    logical, optional,          intent(in)  :: ext
    !!----
    !!----    Calculate the star of the propagation vector
    !!----    and the group of the vector k, Gk, as well as the extended
    !!----    little group Gk,-k. Construct the object "Groupk" that has
    !!----    as components: SpaceGroup + Pointers to the
    !!----    operators belonging to Gk, Gk,-k and the star of k.
    !!----
    !!---- Updates: February - 2005, October - 2016
    !!
    Subroutine K_Star(K,Spacegroup,Gk,ext)
       !---- Arguments ----!
       real(kind=cp), dimension(3), intent(in) :: k
       Type (SpG_Type),             intent(in) :: SpaceGroup
       Type (Group_k_Type),         intent(out):: Gk
       logical, optional,           intent(in) :: ext

       !---- Local Variables ----!
       real(kind=cp), dimension(3)  :: h
       integer, dimension(48)       :: ind   !The maximum number of rotational elements is 48 (=order of the point group m-3m)
       integer                      :: i, j, m, l, ng

       ng=SpaceGroup%numops  !Reduced set of symmetry operators (the firsts of the list: there is no centring translation nor inversion centre)
       Gk%co=0               !Initializing pointers
       Gk%p=0
       ind=1
       Gk%g0=SpaceGroup  !<- Copy the whole space group to the component G0 of Gk
       Gk%p(1)= 1        !<- pointer to the identity that always belong to G_k
       Gk%p(192)= 1      !<- pointer to the identity that always is a coset representative of G0[Gk]
       Gk%nk  = 1
       Gk%ngk = 1
       Gk%stark(:,1) = k  !<- First arm of the star of k
       Gk%k_equiv_minusk = .true. !it is supposed that k equiv -k
       Gk%minusk=.false.
       Gk%extended=.false.

       !Test if there is an operator mapping k to -k
       if (SpaceGroup%Centred /= 1) then  !Centric space group (centric: 0, 2, acentric:1)
          Gk%minusk=.true.
          j=ng+1      !the operator SpaceGroup%SymOp(j) is the inversion centre
          h=matmul(k,real(SpaceGroup%Op(j)%Mat(1:3,1:3)))  !h is here -k because only the rotational part of SpaceGroup%SymOp(j)
                           !is applied to k and this rotational part is always -I (I:identity matrix)
          !h=-k  !This instruction can replace the above one
          !---- k not equivalent to -k
          if (.not. k_EQUIV(h,k, SpaceGroup%SPG_lat))  Gk%k_equiv_minusk = .false.
          ng=ng*2  !Now ng is updated to count also the symmetry elements of SpaceGroup in which
                   !there are the operators transformed by the inversion centre.
       else   !A-centric
         do j=2,ng
           h = matmul(k,real(SpaceGroup%Op(j)%Mat(1:3,1:3)))
           if (k_EQUIV(h,-k, SpaceGroup%SPG_lat))  then
            Gk%minusk=.true.
           end if
         end do
         h=-k
         if (.not. k_EQUIV(h,k, SpaceGroup%SPG_lat))  Gk%k_equiv_minusk = .false.
         !if(.not. Gk%minusk) Gk%k_equiv_minusk = .false.
       end if

       do_ng: do i=2,ng   !From 2 because the first element is always the identity and k is equivalent to k!
                          !We discard the operators containing centring translations with ordering
                          !numbers > 2 * SpaceGroup%numops if there is an inversion centre
          h = matmul(k,real(SpaceGroup%Op(i)%Mat(1:3,1:3)))

          !---- this operation belong to Gk
          if (k_EQUIV(h,k, SpaceGroup%SPG_lat)) then !h is equivalent to k, so the current symmetry operator "i", belongs to Gk
             Gk%ngk = Gk%ngk +1    !Increase the number of elements of Gk
             Gk%p(Gk%ngk)      = i !The operator "i" belongs to Gk
             Gk%co(Gk%ngk,1)   = i !First column of matrix CO (coset representatives of Gk)
             cycle do_ng
          end if
          !---- Passing here means that h is not equivalent to k, so there is eventually a
          !-----new k-vector (=h) to be added to the list of the k-star.
          !---- Look if the generated h-vector is already equivalent to
          !---- one of the list
          do j=1, Gk%nk
             if (k_EQUIV(h,Gk%stark(:,j), SpaceGroup%SPG_lat) ) then
                ind(j)=ind(j)+1      !Increase the counter
                Gk%co(ind(j),j) = i  !Store the operator corresponding to the co-set of the equivalet to h.
                cycle do_ng
             end if
          end do
          !---- Passing here is to add a new vector to the list
          Gk%nk = Gk%nk +1             !this construct the star of k, increase the number of arms of the star
          Gk%stark(:,Gk%nk) = h        !Store the new k-vector belonging to the star
          Gk%p(193-Gk%nk)   = i        !This points to operators changing k
          Gk%co(1,Gk%nk)    = i        !First of the co-set representatives
       end do do_ng

       if(present(ext) .and. .not. Gk%k_equiv_minusk .and. Gk%minusk) then
         m=Gk%ngk
         Gk%extended=.true.
         do_ext: do i=2, Gk%nk
            h= Gk%stark(:,i)
            if (k_EQUIV(h,-k, Gk%G0%SPG_lat)) then
              do l=1,48
                j=Gk%co(l,i)
                if (j == 0) exit do_ext
                m=m+1
                Gk%p(m)=j
              end do
            end if
         end do do_ext
       end if

       Gk%Eqv_k(:,1)=k
       do l=2,48
          j=Gk%co(l,1)
          if (j == 0) exit
          h = matmul(k,real(Gk%G0%Op(j)%Mat(1:3,1:3)))
          Gk%Eqv_k(:,l)=h !Equivalent vectors to k1
       end do

    End Subroutine K_Star

    !!----
    !!---- Subroutine Set_Gk(Gk,SPGk,ext)
    !!----    Type (Group_k_Type),      intent(in) :: Gk
    !!----    Type (SpG_Type),          intent(out):: SPGk
    !!----    logical, optional,        intent(in) :: ext
    !!----
    !!----    Subroutine to convert the propagation vector group Gk,
    !!----    or the extended little group Gk,-k to a conventional space group.
    !!----    When ext is present is the extended little group that is converted
    !!----    to a conventional space group type.
    !!----
    !!---- Updates: April - 2015, October 2016
    !!
    Subroutine Set_Gk(Gk,SPGk,ext)
       !---- Arguments ----!
       Type (Group_k_Type),          intent(in)  :: Gk
       class (SpG_Type),allocatable, intent(out) :: SPGk
       logical, optional,            intent(in)  :: ext

       !---- Local Variables ----!
       character(len=50),dimension(Gk%G0%multip) :: gen
       integer                                   :: i, j, ng, ngen,e_numops,e0_numops


       ng=Gk%G0%numops

       e_numops=ng
       e0_numops=Gk%ngk

       if(Gk%G0%centred /= 1) then
          e_numops=2*e_numops
          e0_numops=2*e0_numops
       end if

       !Check first if the extended little group is equal to the parent group
       if(e0_numops == e_numops) then
          SPGk=Gk%G0
          return
       end if

       if(Gk%G0%centred /= 1) ng=ng+1
       Ngen=0
       do i=2,Gk%ngk
        j=Gk%p(i)
        if( j <= ng) then
           ngen=ngen+1
           gen(ngen)=Gk%G0%Symb_Op(j)
        else
           exit
        end if
       end do
       if(present(ext) .and. .not. Gk%k_equiv_minusk ) then
          do i=Gk%ngk+1,2*Gk%ngk
            ngen=ngen+1
            j=Gk%p(i)
            if(j /= 0) then
               gen(ngen)=Gk%G0%Symb_Op(j)
            else
               ngen=ngen-1
            end if
          end do
       end if
       !Add now the lattice translations
       Select Case(Gk%G0%SPG_lat)
          Case("A")
             ngen=ngen+1
             gen(ngen)="x,y+1/2,z+1/2"
          Case("B")
             ngen=ngen+1
             gen(ngen)="x+1/2,y,z+1/2"
          Case("C")
             ngen=ngen+1
             gen(ngen)="x+1/2,y+1/2,z"
          Case("I")
             ngen=ngen+1
             gen(ngen)="x+1/2,y+1/2,z+1/2"
          Case("F")
             ngen=ngen+1
             gen(ngen)="x+1/2,y+1/2,z"
             ngen=ngen+1
             gen(ngen)="x+1/2,y,z+1/2"
          Case("R")
             ngen=ngen+1
             gen(ngen)="x+2/3,y+1/3,z+1/3"
       End Select
       allocate(SPG_Type::SPGk)
       SPGk%magnetic=.false.
       Call Set_SpaceGroup(" ",SPGk,Ngen,Gen)
    End Subroutine Set_Gk

    !!----
    !!---- Subroutine Write_Group_K(Gk,Lun)
    !!----    Type (Group_k_Type),   intent(in) :: Gk    !  In ->
    !!----    integer, optional,     intent(in) :: lun   !  In ->
    !!----
    !!----    Subroutine to write the operators of the propagation vector
    !!----    group and the list of all vectors {k}, belonging to the star
    !!----    of k.
    !!----
    !!---- Update: February - 2005
    !!
    Subroutine Write_Group_k(Gk,lun)
       !---- Arguments ----!
       Type (Group_k_Type),   intent(in) :: Gk
       Integer, optional,     intent(in) :: lun

       !---- Local Variables ----!
       real(kind=cp), dimension(3)  :: h,k
       character(len=4)             :: kvec
       character(len=:),allocatable :: formato
       integer                      :: i, j, lu, l, m,n_ext

       lu=6
       if (present(lun)) lu=lun

       write(unit=lu,fmt="(/,a)") "      PROPAGATION VECTOR GROUP INFORMATION"
       write(unit=lu,fmt="(a,/)") "      ===================================="

       h=Gk%stark(:,1)
       m=Gk%ngk
       if(.not. Gk%k_equiv_minusk .and. Gk%minusk) m=m*2
       n_ext=m
       write(unit=lu,fmt="(a,3f8.4,a)") " => The input propagation vector is: K=(",h," )"
       if (Gk%k_equiv_minusk) then
          write(unit=lu,fmt="(a,i3,a)")    " =>  K .. IS .. equivalent to -K, the extended little group is G(k) "
       else if (Gk%minusk) then
          write(unit=lu,fmt="(a,i3,a)")    " =>  K .. IS NOT .. equivalent to -K, the extended little group is G(k,-k) "
       else
          write(unit=lu,fmt="(a,i3,a)")    " => -K .. IS NOT .. in the star of k, the extended little group is G(k,-k)=G(k) "
       end if
       write(unit=lu,fmt="(a)")           " => The operators following the k-vectors constitute the co-set decomposition G[Gk]"
       write(unit=lu,fmt="(a)    ")       "    The list of equivalent k-vectors are also given on the right of operators.     "

       if(Gk%extended) then
         if(m > 1) then
           write(unit=lu,fmt="(a,48(i3,a))")  " => Numerals of the extended little group operators:  {",(Gk%p(j),",",j=1,m-1),Gk%p(m)," }"
         else
           write(unit=lu,fmt="(a,i3,a)")  " => Numerals of the extended little group operators:  {",Gk%p(1),"}"
         end if
       end if

       write(unit=lu,fmt="(a,i3,a,/)")" => The star of K is formed by the following ",Gk%nk," vectors:"

       m=0
       do i=1,Gk%G0%multip
          if(len_trim(Gk%G0%Symb_Op(i)) > m ) m=len_trim(Gk%G0%Symb_Op(i))
       end do
       formato="(a,i3,a,a  ,a,3f8.4,a)"
       write(unit=formato(10:11),fmt="(i2)") m
       do i=1, Gk%nk
          j=Gk%p(193-i)
          kvec="    "
          if (i < 10) then
             write(unit=kvec,fmt="(a,i1)")"k_",i
          else
             write(unit=kvec,fmt="(a,i2)")"k_",i
          end if
          k= Gk%stark(:,i)
          if (.not. k_EQUIV(-h,k, Gk%G0%SPG_lat)) then
             write(unit=lu,fmt="(/,a,3f8.4,a,i3,a,a)") "           "//kvec//" = (",k,  &
                   " )    Op: (",j,") ",trim(Gk%G0%Symb_Op(j))
          else
             write(unit=lu,fmt="(/,a,3f8.4,a,i3,a,a)") "  Eqv. -K: "//kvec//" = (",k,  &
                   " )    Op: (",j,") ",trim(Gk%G0%Symb_Op(j))
          end if
          do l=2,48
             j=Gk%co(l,i)
             if (j == 0) exit
             k = matmul(h,real(Gk%G0%Op(j)%Mat(1:3,1:3)))
             write(unit=lu,fmt=formato)"                                                 Op: (",  &
                                        j,") ",     Gk%G0%Symb_Op(j) ,"  -> ( ",k," )"
          end do
       end do

       write(unit=lu,fmt="(/,a,/)")    " => G_k has the following symmetry operators: "

       do i=1, Gk%ngk
          j=Gk%p(i)
          write(unit=lu,fmt="(2(a,i3),a,a)") "    ",i," SYMM(",j,") = ", trim(Gk%G0%Symb_Op(j))
       end do

       if(Gk%extended) then
          write(unit=lu,fmt="(/,a,/)")    " => The extended little group G(k,-k) has the following symmetry operators: "
          do i=1, n_ext
             j=Gk%p(i)
             write(unit=lu,fmt="(2(a,i3),a,a)") "    ",i," SYMM(",j,") = ", trim(Gk%G0%Symb_Op(j))
          end do
       end if

       if (.not. Gk%k_equiv_minusk .and. Gk%G0%Centred == 1) then
          write(unit=lu,fmt="(/,a)")    " => The original space group is non-centrosymmetric and K is not equivalent to -K"
          write(unit=lu,fmt="(a  )")    "    If the vector k=-K does not appear within the arm of the K-vector, the propagation"
          write(unit=lu,fmt="(a  )")    "    vector k=-K should be included in applications where final vector components have "
          write(unit=lu,fmt="(a,/)")    "    to be real values.   "
       end if

    End Subroutine Write_Group_k

 End Module CFML_Propagation_Vectors
