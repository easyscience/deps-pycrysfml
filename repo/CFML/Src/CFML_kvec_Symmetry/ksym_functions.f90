SubModule (CFML_kvec_Symmetry) ksym_functions
   implicit none
   Contains
    !-------------------!
    !---- Functions ----!
    !-------------------!

    !!--++
    !!--++ Module Logical Function Is_Xyz(A) Result(Iss_Xyz)
    !!--++    character(len=*), intent(in) :: A
    !!--++
    !!--++    (PRIVATE)
    !!--++    Determine if A is a character X, Y or Z
    !!--++
    !!--++ Update: February - 2005
    !!
    Module Function Is_Xyz(A) Result(Iss_Xyz)
       !---- Argument ----!
       character(len=*), intent(in) :: A
       logical                      :: Iss_xyz

       Iss_xyz=.false.
       if (A == "x" .or. A == "X" .or.   &
           A == "y" .or. A == "Y" .or.   &
           A == "z" .or. A == "Z")  Iss_xyz=.true.

    End Function Is_Xyz
    !!----
    !!---- Module Function Applyso(Op,V) Result(Applysop)
    !!----    Type(Sym_Oper_Type),          intent(in) :: Op        !  In -> Symmetry Operator Type
    !!----    real(kind=cp), dimension(3) , intent(in) :: v         !  In -> Vector
    !!----    real(kind=cp), dimension(3)              :: ApplySOp  ! Out -> Output vector
    !!----
    !!----    Apply a symmetry operator to a vector:  Vp = ApplySO(Op,v)
    !!----
    !!---- Update: February - 2005
    !!
    Module Function ApplySO(Op,V) Result(Applysop)
       !---- Arguments ----!
       Type(Sym_Oper_Type),          intent(in) :: Op
       real(kind=cp), dimension(3),  intent(in) :: v
       real(kind=cp), dimension(3)              :: ApplySOp

       ApplySOp = matmul(Op%Rot,v) + Op%tr
    End Function ApplySO
    !!----
    !!---- Module Function ApplyMso(Op,Sk) Result(Skp)
    !!----    Type(MSym_Oper_Type),   intent(in) :: Op        !  Magnetic Symmetry Operator Type
    !!----    complex, dimension(3) , intent(in) :: Sk        !  Complex vector
    !!----    complex, dimension(3)              :: Skp       !  Transformed complex vector
    !!----
    !!----    Apply a magnetic symmetry operator to a complex vector:  Skp = ApplyMSO(Op,Sk)
    !!----
    !!---- Update: April - 2005
    !!
    Module Function ApplyMSO(Op,Sk) Result(Skp)
       !---- Arguments ----!
       Type(MSym_Oper_Type), intent(in) :: Op
       Complex, dimension(3),intent(in) :: Sk
       Complex, dimension(3)            :: Skp

       Skp = matmul(Op%Rot,Sk) * cmplx(cos(tpi*Op%Phas),sin(tpi*Op%Phas))

    End Function ApplyMSO

    !!----
    !!---- Module Logical Function Lattice_Trans(V,Lat) Result(Lattice_Transl)
    !!----    real(kind=cp), dimension(3), intent( in) :: v              !  In -> Vector
    !!----    character(len=*),            intent( in) :: Lat            !  In -> Lattice Character
    !!----    logical                                  :: Lattice_Transl ! Out -> .True. or .False.
    !!----
    !!----    Determine whether a vector is a lattice vector
    !!----    depending on the Bravais lattice.
    !!----
    !!---- Update: February - 2005
    !!
    Module Function Lattice_Trans(V,Lat) Result(Lattice_Transl)
       !---- Argument ----!
       real(kind=cp), dimension(3), intent( in) :: v
       character(len=*),            intent( in) :: Lat
       logical                                  :: Lattice_Transl

       !---- Local variables ----!
       real(kind=cp)   , dimension(3) :: vec
       integer                        :: i,nlat

       Lattice_Transl=.false.

       if (Zbelong(v)) then                      ! if v is an integral vector =>  v is a lattice vector
          Lattice_Transl=.true.
       else                                      ! if not look for lattice type
          select case (Lat)
             case("A","a")
                vec=Ltr_a(:,2)-v
                if (Zbelong(vec)) Lattice_Transl=.true.
             case("B","b")
                vec=Ltr_b(:,2)-v
                if (Zbelong(vec)) Lattice_Transl=.true.
             case("C","c")
                vec=Ltr_c(:,2)-v
                if (Zbelong(vec)) Lattice_Transl=.true.
             case("I","i")
                vec=Ltr_i(:,2)-v
                if (Zbelong(vec)) Lattice_Transl=.true.
             case("R","r")
                vec=Ltr_r(:,2)-v
                if (Zbelong(vec)) Lattice_Transl=.true.
                vec=Ltr_r(:,3)-v
                if (Zbelong(vec)) Lattice_Transl=.true.
             case("F","f")
                vec=Ltr_f(:,2)-v
                if (Zbelong(vec)) Lattice_Transl=.true.
                vec=Ltr_f(:,3)-v
                if (Zbelong(vec)) Lattice_Transl=.true.
                vec=Ltr_f(:,4)-v
                if (Zbelong(vec)) Lattice_Transl=.true.
             case("Z")
                do i=2,nlat
                  vec=Ltr(:,i)-v
                  if (Zbelong(vec)) then
                    Lattice_Transl=.true.
                    exit
                  end if
                end do
          end select
       end if
    End Function  Lattice_Trans

    !!----
    !!---- Module Function Veclength(A,B) Result(c)
    !!----    real(kind=cp), dimension(3,3), intent(in)  :: a
    !!----    real(kind=cp), dimension(3),   intent(in)  :: b
    !!----    real(kind=cp),                             :: c
    !!----
    !!----    Length of vector B when A is the Crystallographic
    !!----    to orthogonal matrix, length=c
    !!----
    !!---- Update: February - 2005
    !!
    Module Function Veclength(a,b) Result(c)
       !---- Arguments ----!
       real(kind=cp), intent(in)  , dimension(3,3) :: a
       real(kind=cp), intent(in)  , dimension(3  ) :: b
       real(kind=cp)                               :: c

       !---- Local variables ----!
       integer                     :: i,j
       real(kind=cp), dimension(3) :: v

       v=0.0
       do i = 1,3
          do j = 1,3
             v(i) = v(i)+a(i,j)*b(j)
          end do
       end do
       c = sqrt(v(1)*v(1)+v(2)*v(2)+v(3)*v(3))
    End Function Veclength

End SubModule ksym_functions