SubModule (CFML_gSpaceGroups) gS_Get_X_Matrix
   implicit none
   Contains

   !!----
   !!---- GET_A_MATRICES_CRYS
   !!----
   !!---- Build A matrices -see Acta Cryst. A55 383-395 (1999) -.
   !!----
   !!---- 22/04/2019
   !!
   Module Subroutine Get_A_Matrix_Crys(Laueclass,A,N)
       !---- Arguments ----!
       character(len=*),                 intent(in)  :: LaueClass
       type(rational), dimension(3,3,6), intent(out) :: A
       integer,                          intent(out) :: n

       !---- Local variables ----!
       integer                          :: i
       type(rational), dimension(3,3)   :: R1,R2,R3
       type(rational), dimension(3,3,6) :: Ainv


       select case (trim(LaueClass))
           case ("2/m")
               n  = 6
               R1(1,1:3) = [ 1//1, 0//1, 0//1 ]
               R1(2,1:3) = [ 0//1, 1//1, 0//1 ]
               R1(3,1:3) = [ 0//1, 0//1, 1//1 ]
               R2(1,1:3) = [ 0//1, 0//1, 1//1 ]
               R2(2,1:3) = [ 0//1,-1//1, 0//1 ]
               R2(3,1:3) = [ 1//1, 0//1, 0//1 ]
               R3(1,1:3) = [-1//1, 0//1, 1//1 ]
               R3(2,1:3) = [ 0//1, 1//1, 0//1 ]
               R3(3,1:3) = [-1//1, 0//1, 0//1 ]

               Ainv(:,:,1) = R1(:,:)
               Ainv(:,:,2) = R3(:,:)
               Ainv(:,:,3) = matmul(R3,R3)
               Ainv(:,:,4) = R2
               Ainv(:,:,5) = matmul(R2,R3)
               Ainv(:,:,6) = matmul(Ainv(:,:,5),R3)

           case ("mmm")
               n  = 6
               R1(1,1:3) = [ 1//1, 0//1, 0//1 ]
               R1(2,1:3) = [ 0//1, 1//1, 0//1 ]
               R1(3,1:3) = [ 0//1, 0//1, 1//1 ]
               R2(1,1:3) = [ 0//1, 1//1, 0//1 ]
               R2(2,1:3) = [ 1//1, 0//1, 0//1 ]
               R2(3,1:3) = [ 0//1, 0//1,-1//1 ]
               R3(1,1:3) = [ 0//1, 0//1, 1//1 ]
               R3(2,1:3) = [ 1//1, 0//1, 0//1 ]
               R3(3,1:3) = [ 0//1, 1//1, 0//1 ]

               Ainv(:,:,1) = R1(:,:)
               Ainv(:,:,2) = R3(:,:)
               Ainv(:,:,3) = matmul(R3,R3)
               Ainv(:,:,4) = R2
               Ainv(:,:,5) = matmul(R2,R3)
               Ainv(:,:,6) = matmul(Ainv(:,:,5),R3)

           case ("m-3")
               n = 2
               R1(1,1:3) = [ 1//1, 0//1, 0//1 ]
               R1(2,1:3) = [ 0//1, 1//1, 0//1 ]
               R1(3,1:3) = [ 0//1, 0//1, 1//1 ]
               R2(1,1:3) = [ 0//1,-1//1, 0//1 ]
               R2(2,1:3) = [ 1//1, 0//1, 0//1 ]
               R2(3,1:3) = [ 0//1, 0//1, 1//1 ]
               Ainv(:,:,1) = R1(:,:)
               Ainv(:,:,2) = R2(:,:)

           case default
               n = 1
               R1(1,1:3) = [ 1//1, 0//1, 0//1 ]
               R1(2,1:3) = [ 0//1, 1//1, 0//1 ]
               R1(3,1:3) = [ 0//1, 0//1, 1//1 ]
               Ainv(:,:,1) = R1(:,:)
       end select

       do i = 1 , n
           A(:,:,i)=Rational_Inverse_Matrix(Ainv(:,:,i))
       end do
   End Subroutine Get_A_Matrix_Crys

   !!----
   !!---- Get_A_Matrices_Shub
   !!----
   !!---- Similar to Get_A_Matrices_Crys, but adapted to Shubnikov groups
   !!----
   !!---- 22/04/2019
   !!
   Module Subroutine Get_A_Matrix_Shub(Laueclass,A,N)
      !---- Arguments ----!
      character(len=*),                 intent(in)  :: laueClass
      type(rational), dimension(3,3,6), intent(out) :: A
      integer,                          intent(out) :: n


      n = 1

      !> setting a,b,c
      A(1,1:3,1) = [ 1, 0, 0 ]
      A(2,1:3,1) = [ 0, 1, 0 ]
      A(3,1:3,1) = [ 0, 0, 1 ]

      select case (trim(laueClass))
          case ("2/m") ! Monoclinic
             n = 6

             !> setting c,b,-a-c
             A(1,1:3,2) = [ 0, 0,-1 ]
             A(2,1:3,2) = [ 0, 1, 0 ]
             A(3,1:3,2) = [ 1, 0,-1 ]

             !> setting -a-c,b,a
             A(1,1:3,3) = [-1, 0, 1 ]
             A(2,1:3,3) = [ 0, 1, 0 ]
             A(3,1:3,3) = [-1, 0, 0 ]

             !> setting c,-b,a
             A(1,1:3,4) = [ 0, 0, 1 ]
             A(2,1:3,4) = [ 0,-1, 0 ]
             A(3,1:3,4) = [ 1, 0, 0 ]

             !> setting -a-c,-b,c
             A(1,1:3,5) = [-1, 0, 0 ]
             A(2,1:3,5) = [ 0,-1, 0 ]
             A(3,1:3,5) = [-1, 0, 1 ]

             !> setting a,-b,-a-c
             A(1,1:3,6) = [ 1, 0,-1 ]
             A(2,1:3,6) = [ 0,-1, 0 ]
             A(3,1:3,6) = [ 0, 0,-1 ]

          case ("mmm") ! Orthorhombic
             n = 6

             !> setting b,c,a
             A(1,1:3,2) = [ 0, 0, 1 ]
             A(2,1:3,2) = [ 1, 0, 0 ]
             A(3,1:3,2) = [ 0, 1, 0 ]

             !> setting c,a,b
             A(1,1:3,3) = [ 0, 1, 0 ]
             A(2,1:3,3) = [ 0, 0, 1 ]
             A(3,1:3,3) = [ 1, 0, 0 ]

             !> setting a,c,-b
             A(1,1:3,4) = [ 1, 0, 0 ]
             A(2,1:3,4) = [ 0, 0,-1 ]
             A(3,1:3,4) = [ 0, 1, 0 ]

             !> setting c,b,-a
             A(1,1:3,5) = [ 0, 0,-1 ]
             A(2,1:3,5) = [ 0, 1, 0 ]
             A(3,1:3,5) = [ 1, 0, 0 ]

             !> setting b,a,-c
             A(1,1:3,6) = [ 0, 1, 0 ]
             A(2,1:3,6) = [ 1, 0, 0 ]
             A(3,1:3,6) = [ 0, 0,-1 ]

          case ("-3","-3 R","-3m","-3m R","-3m1","-31m") ! Trigonal
             n = 2

             !> reverse -> obverse setting
             A(1,1:3,2) = [-1, 0, 0 ]
             A(2,1:3,2) = [ 0,-1, 0 ]
             A(3,1:3,2) = [ 0, 0, 1 ]
      end select
   End Subroutine Get_A_Matrix_Shub

   !!----
   !!---- GET_MC_MATRIX
   !!----
   !!---- Computes a correction matrix if necessary. A correction needed
   !!---- in some cases for cubic groups with primitive lattices has been
   !!---- introduced in the subroutine Get_A_Matrices_Crys, for the case m-3.
   !!----
   !!---- 22/04/2019
   !!
   Module Function Get_Mc_Matrix(LaueClass, Mp) Result(Mc)
      !---- Arguments ----!
      character(len=*),               intent(in)  :: LaueClass
      type(rational), dimension(3,3), intent(in)  :: Mp
      type(rational), dimension(3,3)              :: Mc

      !---- Local variables ----!
      character                      :: lattyp
      character(len=60)              :: symb
      type(rational), dimension(3,3) :: Mcinv
      type(rational), dimension(4,4) :: McAux
      logical                        :: pout

      !>==== DEBUG ====
      pout=.false.
      pout= (pout .or. CFML_DEBUG)
      !>===============

      if (pout) then
         write(*,'(8x,a)') " => Constructing (Mc,0) matrix...."
         write(*,'(8x,a)') "    This matrix corrects (M',0) if (M',0) does not "// &
                           "bring the system to a standard setting"
      end if

      select case (trim(LaueClass))
         case ("2/m")  ! Put the two fold axis along b

            Mcinv(1,:) = [ 0//1, 1//1, 0//1 ]
            Mcinv(2,:) = [ 0//1, 0//1, 1//1 ]
            Mcinv(3,:) = [ 1//1, 0//1, 0//1 ]
            Mc=Rational_Inverse_Matrix(Mcinv)

         case ("4/m","4/mmm")
            lattyp=Get_Lattice_Type(Mp)
            if (Err_CFML%Ierr /= 0) return

            if (lattyp == "C") then  ! C -> P
               Mcinv(1,:) = [ 1//1, 1//1, 0//1 ]
               Mcinv(2,:) = [ 1//1,-1//1, 0//1 ]
               Mcinv(3,:) = [ 0//1, 0//1,-1//1 ]
               Mc=Rational_Inverse_Matrix(Mcinv)

            else if (lattyp == "F") then ! F -> I
               Mcinv(1,:) = [ 1//1, 1//1, 0//1 ]
               Mcinv(2,:) = [-1//1, 1//1, 0//1 ]
               Mcinv(3,:) = [ 0//1, 0//1, 1//1 ]
               Mc=Rational_Inverse_Matrix(Mcinv)

            else
               Mc(1,:) = [ 1//1, 0//1, 0//1 ]
               Mc(2,:) = [ 0//1, 1//1, 0//1 ]
               Mc(3,:) = [ 0//1, 0//1, 1//1 ]
            end if

         case ("-3","-3 R")
            lattyp=Get_Lattice_Type(Mp)
            if (Err_CFML%Ierr /= 0) return

            if (lattyp == "S") then ! reverse -> obverse setting
               Mc(1,:) = [-1//1, 0//1, 0//1 ]
               Mc(2,:) = [ 0//1,-1//1, 0//1 ]
               Mc(3,:) = [ 0//1, 0//1, 1//1 ]

            else
               Mc(1,:) = [ 1//1, 0//1, 0//1 ]
               Mc(2,:) = [ 0//1, 1//1, 0//1 ]
               Mc(3,:) = [ 0//1, 0//1, 1//1 ]
            end if

         case ("-3m","-3m R","-3m1","-31m")
            lattyp=Get_Lattice_Type(Mp)
            if (Err_CFML%Ierr /= 0) return

            if (lattyp == "S") then ! reverse -> obverse setting
               Mc(1,:) = [-1//1, 0//1, 0//1 ]
               Mc(2,:) = [ 0//1,-1//1, 0//1 ]
               Mc(3,:) = [ 0//1, 0//1, 1//1 ]

            else if (lattyp == "H") then ! H -> P
               Mcinv(1,:) = [ 1//1, 1//1, 0//1 ]
               Mcinv(2,:) = [-1//1, 2//1, 0//1 ]
               Mcinv(3,:) = [ 0//1, 0//1, 1//1 ]
               Mc=Rational_Inverse_Matrix(Mcinv)

            else
               Mc(1,:) = [ 1//1, 0//1, 0//1 ]
               Mc(2,:) = [ 0//1, 1//1, 0//1 ]
               Mc(3,:) = [ 0//1, 0//1, 1//1 ]
            end if

         case ("6/mmm")
            lattyp=Get_Lattice_Type(Mp)
            if (Err_CFML%Ierr /= 0) return

            if (lattyp == "H") then ! H -> P
               Mcinv(1,:) = [ 1//1, 1//1, 0//1 ]
               Mcinv(2,:) = [-1//1, 2//1, 0//1 ]
               Mcinv(3,:) = [ 0//1, 0//1, 1//1 ]
               Mc=Rational_Inverse_Matrix(Mcinv)

            else
               Mc(1,:) = [ 1//1, 0//1, 0//1 ]
               Mc(2,:) = [ 0//1, 1//1, 0//1 ]
               Mc(3,:) = [ 0//1, 0//1, 1//1 ]
            end if

         case default
            Mc(1,:) = [ 1//1, 0//1, 0//1 ]
            Mc(2,:) = [ 0//1, 1//1, 0//1 ]
            Mc(3,:) = [ 0//1, 0//1, 1//1 ]

      end select

      if (pout) then
         write(*,'(12x,a)',advance='no') "Quasi-standard setting --> Standard setting transformation: "
         call set_identity_matrix(4)
         McAux = identity_matrix
         McAux(1:3,1:3) = Mc
         symb=Get_Symb_from_Mat(transpose(McAux),"abc")
         write(*,'(a)') trim(symb)
      end if
   End Function Get_Mc_Matrix

   !!----
   !!---- GET_S_MATRIX
   !!----
   !!---- Given a rotation matrix W, computes the matrix S defined as:
   !!----           S = W + W^2 + W^3 + ... + W^n
   !!---- where n is the order of the rotation axis. This matrix is
   !!---- used to find vectors perpendicular to the rotation axis. For
   !!---- a vector x perpendicular to the rotation axis, since Sx = 0.
   !!----
   !!---- 22/04/2019
   !!
   Module Function Get_S_Matrix(W) Result(S)
      !---- Arguments ----!
      type(rational), dimension(3,3), intent(in)  :: W
      type(rational), dimension(3,3)               :: S

      !---- Local variables ----!
      integer                        :: i,order
      type(rational), dimension(3,3) :: Waux

      !> Init
      order=abs(Get_Rotation_Order(W))

      S    = W
      Waux = W

      do i = 2 , order
         Waux = MatMul(Waux,W)
         S    = S + Waux
      end do
   End Function Get_S_Matrix

   !!----
   !!---- GET_P_MATRIX
   !!----
   !!---- It returns a 3x3 matrix P which transforms the
   !!---- current setting to a primitive setting
   !!----
   !!---- 22/04/2019
   !!
   Module Function Get_P_Matrix(G,Nospin) Result(P)
      !---- Arguments ----!
      class(spg_type),                intent(in)  :: G
      logical, optional,              intent(in)  :: nospin
      type(rational), dimension(3,3)              :: P

      !---- Local variables ----!
      integer                                     :: i,j,k,n,m
      integer                                     :: nLatt,nAuxVec,nCentringVec
      logical                                     :: primitive,linearDependence,sorted
      character(len=256)                          :: symb
      type(rational)                              :: det,rNumLat
      type(rational), dimension(3)                :: nullVec
      type(rational), dimension(4)                :: v
      type(rational), dimension(3,3)              :: Pinv,A
      type(rational), dimension(4,4)              :: PAux
      type(rational), dimension(:,:), allocatable :: auxVec,centringVec
      logical                                     :: pout

      !>==== DEBUG ====
      pout=.false.
      pout= (pout .or. CFML_DEBUG)
      !>===============

      if (pout) then
         write(*,'(8x,a)') " => Constructing (P,0) matrix...."
         write(*,'(12x,a)') "This matrix transforms the original basis in a primitive basis"
      end if

      !> Init
      call Clear_Error()
      primitive   = .false.
      nullVec(:)  = 0//1

      nLatt = 1
      do i = 1 , G%num_lat
         if (.not. Rational_Equal(G%Lat_tr(:,i),nullVec))  nLatt = nLatt + 1
      end do
      if (present(nospin)) then
         do i = 1 , G%num_alat
            if (.not. Rational_Equal(G%aLat_tr(:,i),nullVec)) nLatt = nLatt + 1
         end do
      end if

      if (nLatt == 1) then
         !> Basis is already primitive
         if (pout) write(*,'(12x,a)') "The basis is already primitive"
         call Rational_Identity_Matrix(P)
         primitive = .true.
         return

      else
         !> Build an expanded list of centring vectors
         nAuxVec = 0
         if (present(nospin)) then
            rNumLat = (G%num_lat+G%num_alat+1)//1
            allocate(auxVec(3,G%num_lat+G%num_alat))

         else
            rNumLat = (G%num_lat+1)//1
            allocate(auxVec(3,G%num_lat))
         end if
         do i = 1 , G%num_lat
            if (.not. Rational_Equal(G%lat_tr(:,i),nullVec)) then
               nAuxVec = nAuxVec + 1
               auxVec(:,nAuxVec) = G%lat_tr(:,i)
            end if
         end do
         if (present(nospin)) then
            do i = 1 , G%num_alat
               if (.not. Rational_Equal(G%alat_tr(:,i),nullVec)) then
                  nAuxVec = nAuxVec + 1
                  auxVec(:,nAuxVec) = G%alat_tr(:,i)
               end if
            end do
         end if
         allocate(centringVec(4,10*nAuxVec))
         nCentringVec = 0
         do n = 1 , nAuxVec
            do i = 0 , 1
               if (i == 1 .and. auxVec(1,n)%numerator == 0) cycle
               v(1)%numerator   = auxVec(1,n)%numerator - i*auxVec(1,n)%denominator
               v(1)%denominator = auxVec(1,n)%denominator
               do j = 0 , 1
                  if (j == 1 .and. auxVec(2,n)%numerator == 0) cycle
                  v(2)%numerator   = auxVec(2,n)%numerator - j*auxVec(2,n)%denominator
                  v(2)%denominator = auxVec(2,n)%denominator
                  do k = 0 , 1
                     if (k == 1 .and. auxVec(3,n)%numerator == 0) cycle
                     v(3)%numerator   = auxVec(3,n)%numerator - k*auxVec(3,n)%denominator
                     v(3)%denominator = auxVec(3,n)%denominator
                     v(4) = dot_product(v(1:3),v(1:3))
                     !> Check if there is linear dependence with previous vectors
                     linearDependence = .False.
                     do m = 1 , nCentringVec
                        if (Rational_Co_Linear(v(1:3),centringVec(1:3,m))) Then
                           linearDependence = .True.
                           if (v(4) < centringVec(4,m)) centringVec(1:4,m) = v(1:4)
                           exit
                        end if
                     end do
                     if (.not. linearDependence) then
                        nCentringVec = nCentringVec + 1
                        centringVec(:,nCentringVec) = v
                     end if
                  end do
               end do
            end do
         end do
      end if

      !> Sort vectors from shorter to longer
      do
         sorted = .True.
         do i = 1 , ncentringVec - 1
            if (centringVec(4,i) > centringVec(4,i+1)) Then
               v = centringVec(:,i)
               centringVec(:,i)   = centringVec(:,i+1)
               centringVec(:,i+1) = v
               sorted = .False.
            end if
         end do
         if (sorted) exit
      end do

      !> Append the unit translations
      centringVec(:,ncentringVec+1) = [ 1//1, 0//1, 0//1, 1//1 ]
      centringVec(:,ncentringVec+2) = [ 0//1, 1//1, 0//1, 1//1 ]
      centringVec(:,ncentringVec+3) = [ 0//1, 0//1, 1//1, 1//1 ]
      ncentringVec = ncentringVec + 3

      !> Combine three vectors until a primitive setting is found
      primitive = .false.
      do i = 1 , nCentringVec
         if (primitive) exit
         do j = i + 1 , nCentringVec
            if (primitive) exit
            do k = j + 1 , nCentringVec
               if (primitive) exit
               P(:,1) = centringVec(1:3,i)
               P(:,2) = centringVec(1:3,j)
               P(:,3) = centringVec(1:3,k)

               call Clear_Error()
               Pinv=Rational_Inverse_Matrix(P)
               if (Err_CFML%ierr == 0) then
                  det = rational_determ(P)
                  if (abs((1//1)/det) == rNumLat) then
                     primitive = .true.
                     if (det < 0//1) then
                        P(:,1) = -P(:,1)
                        Pinv=Rational_Inverse_Matrix(P)
                     end if
                     do n = 1 , G%Multip
                        A = MatMul(G%Op(n)%Mat(1:3,1:3),P)
                        A = MatMul(Pinv,A)
                        if (.not. Rational_Is_Integer(A)) then
                           primitive = .false.
                           exit
                        end if
                     end do
                  end if
               end if
            end do
         end do
      end do

      if (.not. primitive) then
         Err_CFML%Ierr = 1
         Err_CFML%Msg  = "Get_P_Matrix@SPACEG: A primitive basis cannot be found"
         return

      else if (pout) then
         write(*,'(12x,a)',advance='no') "Original setting --> Primitive setting transformation: "
         call Rational_Identity_Matrix(Paux)
         PAux(1:3,1:3) = P
         symb=Get_Symb_from_Mat(transpose(PAux),"abc")
         write(*,'(a)') trim(symb)
      end if
   End Function Get_P_Matrix

   !!----
   !!---- GET_MP_MATRIX
   !!----
   !!---- It returns a 3x3 matrix Mp which transforms the
   !!---- original setting in a setting where basis vectors
   !!---- are along the symmetry axes of the Laue class.
   !!----
   !!----           [ bx1 by1 bz1 ]
   !!----      Mp = [ bx2 by2 bz2 ]
   !!----           [ bx3 by3 bz3 ]
   !!----
   !!---- 24/04/2019
   !!
   Module Function Get_Mp_Matrix(G,P) Result(Mp)
       !---- Arguments ----!
       class(spg_type),                 intent(in)  :: G
       type(rational), dimension(3,3), intent(in)  :: P
       type(rational), dimension(3,3)              :: Mp

       !---- Local variables ----!
       integer            :: i,j,k,n,n_,nCubicAxes
       character(len=256) :: symb
       logical            :: colinear,standard
       integer,           dimension(2)   :: order
       character(len=20), dimension(6)   :: axisName
       type(rational),    dimension(3)   :: bx,by,bz
       type(rational),    dimension(3,3) :: Pinv,W,U,PM,PMinv
       type(rational),    dimension(3,4) :: vPerp,cubicAxes
       type(rational),    dimension(4,4) :: MpAux
       integer,           dimension(:,:), allocatable :: idd
       logical :: pout

       !>==== DEBUG ====
       pout=.false.
       pout= (pout .or. CFML_DEBUG)
       !>===============

       axisName(2) = "twofold"
       axisName(3) = "threefold"
       axisName(4) = "fourfold"
       axisName(6) = "sixfold"

       Pinv=Rational_Inverse_Matrix(P)

       if (pout) then
           write(*,'(8x,a)') " => Constructing (M',0) matrix...."
           write(*,'(12x,a)') "This matrix transforms the primitive "// &
                              "basis in a basis with vectors along the symmetry axes of the Laue class"
           write(*,'(12x,2a)') "Laue Class: ", G%laue
       end if

       select case (trim(G%laue))
          case ("-1")
             Mp = reshape ( [ 1//1,0//1,0//1,&
                              0//1,1//1,0//1,&
                              0//1,0//1,1//1 ],(/3,3/) )

          case ("2/m","4/m","-3","-3 R","6/m")
             select case (trim(G%laue))
                case ("2/m")
                   order(1) = 2

                case ("4/m")
                   order(1) = 4

                case ("-3","-3 R","6/m")
                    order(1) = 3
             end select

             if (pout) write(*,'(12x,3a)') "Searching for the ",trim(axisName(order(1)))," axis..."
             allocate(idd(G%Multip,2))
             call Get_Rotations(G%Multip,G%op(:),order(1),n,idd)
             if (Err_CFML%Ierr /= 0) return

             !> Put the symmetry operation in the primitive setting
             W = MatMul(G%op(idd(1,1))%Mat(1:3,1:3),P)
             W = MatMul(Pinv,W)
             bz=Get_Rotation_Axis(W)
             if (Err_CFML%Ierr /= 0) return

             if (pout) then
                write(*,'(16x,a,i2,",",i2,",",i2,"]")') "Axis direction in the primitive setting: [", bz(1:3)%Numerator
                write(*,'(12x,a)') "Searching for lattice vectors perpendicular to the rotation axis." // &
                                   "Building the complete basis..."
             end if
             vPerp=Get_VecPerp_To_RotAxis(W)
             if (Err_CFML%Ierr /= 0) return

             call Get_Pseudo_Standard_Base(W,vPerp,bz,bx,by)
             Mp(:,1) = bx
             Mp(:,2) = by
             Mp(:,3) = bz
             deallocate(idd)

          case ("4/mmm")
             if (pout) write(*,'(12x,a)') "Searching for the fourfold axis..."
             allocate(idd(G%Multip,2))
             call Get_Rotations(G%Multip,G%op(:),4,n,idd)
             if (Err_CFML%Ierr /= 0) return

             !> Put the symmetry operation in the primitive setting
             W = MatMul(G%op(idd(1,1))%Mat(1:3,1:3),P)
             W = MatMul(Pinv,W)
             bz=Get_Rotation_Axis(W)
             if (Err_CFML%Ierr /= 0) return

             if (pout) then
                 write(*,'(16x,a,i2,",",i2,",",i2,"]")') "Axis direction in the primitive setting: [", bz(:)%Numerator
                 write(*,'(12x,a)') "Searching for the twofold axis..."
             end if

             call Get_Rotations(G%Multip,G%op(:),2,n,idd)
             if (Err_CFML%Ierr /= 0) return

             colinear = .true.
             do i = 1 , n
                !> Put the symmetry operation in the primitive setting
                U = MatMul(G%op(idd(i,1))%Mat(1:3,1:3),P)
                U = MatMul(Pinv,U)
                bx=Get_Rotation_Axis(U)
                if (Err_CFML%Ierr /= 0) return

                if (.not. Rational_Co_Linear(bz,bx)) then
                   colinear = .false.
                   exit
                end if
             end do
             if (colinear) then
                Err_CFML%Ierr = 1
                Err_CFML%Msg  = "Get_Mp_Matrix@SPACEG: "//&
                                "Unable to find a second rotation axis linearly independent from the first"
                return
             end if
             if (pout) write(*,'(16x,a,i2,",",i2,",",i2,"]")') "Axis direction in the primitive setting: [", bx(:)%Numerator
             by = matmul(W,bx)
             Mp(:,1) = bx
             Mp(:,2) = by
             Mp(:,3) = bz
             deallocate(idd)

          case ("-3m","-3m R","-3m1","-31m","6/mmm")
             if (pout) write(*,'(12x,a)') "Searching for the threefold axis..."
             allocate(idd(G%Multip,2))
             call Get_Rotations(G%Multip,G%op(:),3,n,idd)
             if (Err_CFML%Ierr /= 0) return

             !> Put the symmetry operation in the primitive setting
             W = MatMul(G%op(idd(1,1))%Mat(1:3,1:3),P)
             W = MatMul(Pinv,W)
             if (idd(1,2) == -1)  W = -W
             bz=Get_Rotation_Axis(W)
             if (Err_CFML%Ierr /= 0) return

             if (pout) then
                write(*,'(16x,a,i2,",",i2,",",i2,"]")') "Axis direction in the primitive setting: [", bz(:)%Numerator
                write(*,'(12x,a)') "Searching for the twofold axis..."
             end if
             call Get_Rotations(G%Multip,G%op(:),2,n,idd)
             if (Err_CFML%Ierr /= 0) return

             colinear = .true.
             do i = 1 , n
                !> Put the symmetry operation in the primitive setting
                U = MatMul(G%op(idd(i,1))%Mat(1:3,1:3),P)
                U = MatMul(Pinv,U)
                bx=Get_Rotation_Axis(U)
                if (Err_CFML%Ierr /= 0) return

                if (.not. Rational_Co_Linear(bz,bx)) then
                    colinear = .false.
                    exit
                end if
             end do

             if (colinear) then
                 Err_CFML%Ierr = 1
                 Err_CFML%Msg  = "Get_Mp_Matrix@SPACEG: "// &
                                 "Unable to find a second rotation axis linearly independent from the first"
                 return
             end if
             if (pout) write(*,'(16x,a,i2,",",i2,",",i2,"]")') "Axis direction in the primitive setting: [", bx(:)%Numerator
             by = matmul(W,bx)
             Mp(:,1) = bx
             Mp(:,2) = by
             Mp(:,3) = bz
             deallocate(idd)

          case ("mmm")
             if (pout) write(*,'(12x,a)') "Searching for the three twofold axis..."
             allocate(idd(G%Multip,2))
             call Get_Rotations(G%Multip,G%op(:),2,n,idd)
             if (Err_CFML%Ierr /= 0) return

             n_ = 0
             do i = 1 , n
                !> Put the symmetry operation in the primitive setting
                W = MatMul(G%op(idd(i,1))%Mat(1:3,1:3),P)
                W = MatMul(Pinv,W)
                bz=Get_Rotation_Axis(W)
                if (Err_CFML%Ierr /= 0) return

                colinear = .false.
                do j = 1 , n_
                   if (Rational_Co_Linear(bz,Mp(:,j))) colinear = .true.
                end do
                if (.not. colinear) then
                   n_ = n_ + 1
                   Mp(:,n_) = bz
                   if (pout) write(*,'(16x,a,i2,",",i2,",",i2,"]")') "Axis direction in the primitive setting: [", bz(:)%Numerator
                end if
                if (n_ == 3) exit
             end do
             deallocate(idd)

          case ("m3","m-3","m3m","m-3m")
             if (pout) write(*,'(12x,a)') "Searching for the four threefold axes..."
             allocate(idd(G%Multip,2))
             call Get_Rotations(G%Multip,G%op(:),3,n,idd)
             if (Err_CFML%Ierr /= 0) return

             nCubicAxes = 0
             standard   = .true. ! Initially we assume P is a standard basis
             do i = 1 , n
                W = MatMul(G%op(idd(i,1))%Mat(1:3,1:3),P)
                W = MatMul(Pinv,W)
                bz=Get_Rotation_Axis(W)
                if (Err_CFML%Ierr /= 0) return

                colinear = .false.
                do j = 1 , nCubicAxes
                   if (Rational_Co_Linear(bz,cubicAxes(:,j))) colinear = .true.
                end do
                if (.not. colinear) then
                   nCubicAxes = nCubicAxes + 1
                   cubicAxes(:,nCubicAxes) = bz(:)
                   if (pout) write(*,'(16x,a,i2,",",i2,",",i2,"]")') "Axis direction in the primitive setting: [", bz(:)%Numerator
                   if (abs(bz(1)) /= (1//1) .or. &
                       abs(bz(2)) /= (1//1) .or. &
                       abs(bz(3)) /= (1//1)) standard = .false.
                end if
             end do
             if (standard) then
                Mp(:,1) = [ 1//1,0//1,0//1 ]
                Mp(:,2) = [ 0//1,1//1,0//1 ]
                Mp(:,3) = [ 0//1,0//1,1//1 ]

             else
                !> Find a combination of cubicAxes for which the threefold axes are along {111}
                do i = 0 , 1
                   if (standard) exit
                   bx(:) = cubicAxes(:,2) - ((2 * i)//1) * cubicAxes(:,2)
                   do j = 0 , 1
                      if (standard) exit
                      by(:) = cubicAxes(:,3) - ((2 * j)//1) * cubicAxes(:,3)
                      do k = 0 , 1
                         if (standard) exit
                         bz(:) = cubicAxes(:,4) - ((2 * k)//1) * cubicAxes(:,4)
                         Mp(:,1) = (1//2) * (cubicAxes(:,1) + bx)
                         Mp(:,2) = (1//2) * (cubicAxes(:,1) + by)
                         Mp(:,3) = (1//2) * (cubicAxes(:,1) + bz)

                         !> For lattice I, Mp is not integral (there is a lattice
                         !> point at the middle of each diagonal. We must multiply
                         !> by two in order to get the diagonal of the cube
                         if (.not. Rational_Is_Integer(Mp)) Mp = (2//1) * Mp
                         PM = matmul(P,Mp)

                         call Clear_Error()
                         PMinv=Rational_Inverse_Matrix(PM)
                         if (Err_CFML%ierr == 0) then
                            standard = .true.
                            do n_ = 1 , n
                               W = MatMul(G%op(idd(n_,1))%Mat(1:3,1:3),PM)
                               W = MatMul(PMinv,W)
                               if (Rational_Is_Integer(W)) then
                                  bz=Get_Rotation_Axis(W)
                                  if (abs(bz(1)) /= (1//1) .or. &
                                      abs(bz(2)) /= (1//1) .or. &
                                      abs(bz(3)) /= (1//1)) then
                                      standard = .false.
                                      exit
                                  end if
                               end if
                            end do
                         end if
                      end do
                   end do
                end do
             end if
             if (.not. standard) then
                 Err_CFML%Ierr = 1
                 Err_CFML%Msg  = "Get_Mp_Matrix@SPACEG: "// &
                                 "Unable to build a standard setting for the cubic crystal system..."
                 return
             end if
       end select

       call Set_Right_Handedness(Mp)
       if (pout) then
          write(*,'(tr12,a)',advance='no') "Primitive setting --> Quasi-Standard setting transformation: "
          call Set_Identity_Matrix(4)
          MpAux          = identity_matrix
          MpAux(1:3,1:3) = Mp
          symb=Get_Symb_from_Mat(transpose(MpAux),"abc")
          write(*,'(a)') trim(symb)
       end if
   End Function Get_Mp_Matrix

   !!----
   !!---- GET_VECPERP_TO_ROTAXIS
   !!----
   !!---- Given a rotation matrix W, computes the shortest
   !!---- four lattice vectors perpendicular to the rotation
   !!---- axis.
   !!----
   !!---- 24/04/2019
   !!
   Module Function Get_VecPerp_To_RotAxis(W) Result(vPerp)
       !---- Arguments ----!
       type(rational), dimension(3,3), intent(in)  :: W
       type(rational), dimension(3,4)              :: vPerp

       !---- Local variables ----!
       integer        :: i
       integer        :: rnk,row,nzeros
       type(rational) :: det
       type(rational), dimension(3)   :: nullVector
       type(rational), dimension(3,3) :: A,S,U

       !> Init
       call clear_error()
       vPerp= 0//1
       nullVector(:) = 0//1
       det = rational_determ(W)
       if (det%Numerator == det%Denominator) then
          A = W
       else if (det%Numerator == -det%Denominator) then
          A = -W
       else
          Err_CFML%Ierr = 1
          Err_CFML%Msg = "Get_Vecperp_To_Rotaxis@SPACEG:  Determinant is not +-1"
          return
       end if

       S=Get_S_Matrix(A)
       call Rational_RowEchelonForm(S)
       U = S

       !> Check that the rank of the U matrix is one
       rnk=Rational_Rank(U)
       if ( rnk /= 1 ) then
          Err_CFML%Ierr = 1
          Err_CFML%Msg = "Get_Vecperp_To_Rotaxis@SPACEG: Rank of matrix U is not one"
          return
       end if

       !> Find a row different from zeros
       do i = 1 , 3
          if (.not. Rational_Equal(U(i,:),nullVector)) exit
       end do
       row = i

       !> Count the number of zeros of the first row of the echelon matrix
       nzeros = 0
       do i = 1 , 3
          if (U(row,i) == (0//1)) nzeros = nzeros + 1
       end do

       !> Build the four shortest vectors perpendicular to the rotation axis
       select case (nzeros)
          case (0)
             vPerp(1,1) =  1//1
             vPerp(2,1) =  0//1
             vPerp(1,2) =  0//1
             vPerp(2,2) =  1//1
             vPerp(1,3) =  1//1
             vPerp(2,3) =  1//1
             vPerp(1,4) =  1//1
             vPerp(2,4) = -1//1
             do i = 1 , 4
                vPerp(3,i) = -(vPerp(1,i) * U(row,1) + vPerp(2,i) * U(row,2)) / U(row,3)
             end do

          case (1)
             do i = 1 , 3
                if (U(row,i) == (0//1)) exit
             end do
             select case (i)
                case (1)
                   vPerp(1,1) = 1//1
                   vPerp(2,1) = 0//1
                   vPerp(3,1) = 0//1
                   vPerp(1,2) = 0//1
                   vPerp(2,2) = 1//1
                   vPerp(3,2) = - U(row,2) / U(row,3)
                   vPerp(1,3) = 1//1
                   vPerp(2,3) = 1//1
                   vPerp(3,3) = - U(row,2) / U(row,3)
                   vPerp(1,4) = -1//1
                   vPerp(2,4) = 1//1
                   vPerp(3,4) = - U(row,2) / U(row,3)

                case (2)
                   vPerp(1,1) = 0//1
                   vPerp(2,1) = 1//1
                   vPerp(3,1) = 0//1
                   vPerp(1,2) = 1//1
                   vPerp(2,2) = 0//1
                   vPerp(3,2) = - U(row,1) / U(row,3)
                   vPerp(1,3) = 1//1
                   vPerp(2,3) = 1//1
                   vPerp(3,3) = - U(row,1) / U(row,3)
                   vPerp(1,4) = 1//1
                   vPerp(2,4) = -1//1
                   vPerp(3,4) = - U(row,1) / U(row,3)

                case (3)
                   vPerp(1,1) = 0//1
                   vPerp(2,1) = 0//1
                   vPerp(3,1) = 1//1
                   vPerp(1,2) = 1//1
                   vPerp(2,2) = -U(row,1) / U(row,2)
                   vPerp(3,2) = 0//1
                   vPerp(1,3) = 1//1
                   vPerp(2,3) = -U(row,1) / U(row,2)
                   vPerp(3,3) = 1//1
                   vPerp(1,4) = 1//1
                   vPerp(2,4) = -U(row,1) / U(row,2)
                   vPerp(3,4) = -1//1
             end select

          case (2)
             do i = 1 , 3
                if (U(row,i) /= (0//1)) exit
             end do
             select case (i)
                case (1)
                   vPerp(1,:) =  0//1
                   vPerp(2,1) =  1//1
                   vPerp(3,1) =  0//1
                   vPerp(2,2) =  0//1
                   vPerp(3,2) =  1//1
                   vPerp(2,3) =  1//1
                   vPerp(3,3) =  1//1
                   vPerp(2,4) =  1//1
                   vPerp(3,4) = -1//1

                case (2)
                   vPerp(2,:) =  0//1
                   vPerp(1,1) =  1//1
                   vPerp(3,1) =  0//1
                   vPerp(1,2) =  0//1
                   vPerp(3,2) =  1//1
                   vPerp(1,3) =  1//1
                   vPerp(3,3) =  1//1
                   vPerp(1,4) =  1//1
                   vPerp(3,4) = -1//1

                case (3)
                   vPerp(3,:) =  0//1
                   vPerp(1,1) =  1//1
                   vPerp(2,1) =  0//1
                   vPerp(1,2) =  0//1
                   vPerp(2,2) =  1//1
                   vPerp(1,3) =  1//1
                   vPerp(2,3) =  1//1
                   vPerp(1,4) =  1//1
                   vPerp(2,4) = -1//1
             end select
       end select

       do i = 1 , 4
          call Smallest_Integral_Vector(vPerp(:,i))
       end do
   End Function Get_VecPerp_To_RotAxis

End SubModule gS_Get_X_Matrix