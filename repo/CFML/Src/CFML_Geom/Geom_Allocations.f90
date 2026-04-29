!!----
!!----
!!----
!!
 SubModule (CFML_geom) Geom_Allocations
    implicit none
   Contains
    !!----
    !!---- Module Subroutine Allocate_Coordination_Type(nasu,numops,dmax,Max_Coor)
    !!----    integer,       intent(in) :: nasu      !  In -> Number of atoms in asymmetric unit
    !!----    integer,       intent(in) :: numops    !  In -> Number of S.O. excluding lattice centerings
    !!----    real(kind=cp), intent(in) :: dmax      !  In -> Maximun distance to be calculated
    !!----    integer,      intent(out) :: Max_Coor  !  Maximum coordination allowed
    !!----
    !!----    Allocation of Coordination_Type.
    !!----    Should be called before using this module.
    !!----
    !!---- Update: March - 2005
    !!
    Module Subroutine Allocate_Coordination_Type(nasu,numops,dmax,Max_Coor)
       !---- Arguments ----!
       integer,       intent(in) :: nasu
       integer,       intent(in) :: numops
       real(kind=cp), intent(in) :: dmax
       integer,      intent(out) :: Max_Coor

       !---- local variables ----!
       real(kind=cp), parameter :: r_atom=0.4_cp !Radius of a typical atom

       if (allocated(Coord_Info%Coord_Num)) deallocate(Coord_Info%Coord_Num)
       if (allocated(Coord_Info%N_Cooatm))  deallocate(Coord_Info%N_Cooatm)
       if (allocated(Coord_Info%N_Sym))     deallocate(Coord_Info%N_Sym)
       if (allocated(Coord_Info%Dist))      deallocate(Coord_Info%Dist)
       if (allocated(Coord_Info%S_Dist))    deallocate(Coord_Info%S_Dist)
       if (allocated(Coord_Info%Tr_Coo))    deallocate(Coord_Info%Tr_Coo)


       max_coor= (dmax/r_atom)**3
       max_coor=max(max_coor,nasu*numops)

       !---- Assigninmg the new values ----!
       Coord_Info%Natoms=nasu
       Coord_Info%Max_Coor= max_coor

       allocate (Coord_Info%Coord_Num(nasu))
       allocate (Coord_Info%N_Cooatm(max_coor,nasu))
       allocate (Coord_Info%N_Sym(max_coor,nasu))
       allocate (Coord_Info%Dist(max_coor,nasu))
       allocate (Coord_Info%S_Dist(max_coor,nasu))
       allocate (Coord_Info%Tr_Coo(3,max_coor,nasu))

       Coord_Info%Coord_Num=0
       Coord_Info%N_Cooatm =0
       Coord_Info%N_Sym    =0
       Coord_Info%Dist     =0.0
       Coord_Info%S_Dist   =0.0
       Coord_Info%Tr_Coo   =0.0

    End Subroutine Allocate_Coordination_Type

    !!----
    !!---- Module Subroutine Allocate_Point_List(N,Pl,Ier)
    !!----    integer,               intent(in)     :: n      !  In -> Dimension for allocating components of the type
    !!----    type(point_list_type), intent(in out) :: pl     !  In Out-> Type with allocatable components
    !!----    integer,               intent(out)    :: ier    !  Out -> if ier /= 0 an error occurred.
    !!----
    !!----    Allocation of an objet of type Point_List_Type
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine Allocate_Point_List(n,Pl,Ier)
       !---- Arguments ----!
       integer,               intent(in)     :: n
       type(point_list_type), intent(in out) :: pl
       integer,               intent(out)    :: ier

       ier=0
       if (n <= 0) then
          ier=1
          return
       end if

       if ( .not. allocated(pl%nam) ) allocate(pl%nam(n),stat=ier)
       if ( .not. allocated(pl%p) )   allocate(pl%p(n),stat=ier)
       if ( .not. allocated(pl%x) )   allocate(pl%x(3,n),stat=ier)

       pl%nam= " "
       pl%np=0
       pl%p=0
       pl%x=0.0

    End subroutine Allocate_Point_List

    !!----
    !!---- Module Subroutine Deallocate_Coordination_Type()
    !!----
    !!----    Deallocation of Coordination_Type.
    !!----
    !!---- Update: March - 2005
    !!
    Module Subroutine Deallocate_Coordination_Type()

       if (allocated(Coord_Info%Coord_Num)) deallocate(Coord_Info%Coord_Num)
       if (allocated(Coord_Info%N_Cooatm))  deallocate(Coord_Info%N_Cooatm)
       if (allocated(Coord_Info%N_Sym))     deallocate(Coord_Info%N_Sym)
       if (allocated(Coord_Info%Dist))      deallocate(Coord_Info%Dist)
       if (allocated(Coord_Info%S_Dist))    deallocate(Coord_Info%S_Dist)
       if (allocated(Coord_Info%Tr_Coo))    deallocate(Coord_Info%Tr_Coo)

       !---- Assigninmg the new values ----!
       Coord_Info%Natoms=0
       Coord_Info%Max_Coor= 0

    End Subroutine Deallocate_Coordination_Type

    !!----
    !!---- Module Subroutine Deallocate_Point_List(Pl)
    !!----    type(point_list_type), intent(in out) :: pl  !  In Out-> Type with allocatable components
    !!----
    !!----     De-allocation of an objet of type point_list_type
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine Deallocate_Point_List(Pl)
       !---- Arguments ----!
       type(point_list_type), intent(in out) :: pl

       if (allocated(pl%nam) ) deallocate(pl%nam)
       if (allocated(pl%p) )   deallocate(pl%p)
       if (allocated(pl%x) )   deallocate(pl%x)

    End Subroutine Deallocate_Point_List


 End SubModule Geom_Allocations

