!!----
!!---- SUBMODULE CFML_Maths
!!----
!!----
!!
Submodule (CFML_Maths) Math_Determinant
 Contains

    !!----
    !!---- DETERMINANT_C
    !!----    Calculation of de Determinant of a complex array A(N,N)
    !!----
    Module Function Determinant_C(A,n) Result(Det)
       !---- Arguments ----!
       complex(kind=cp), dimension(:,:), intent(in) :: A
       integer,                          intent(in) :: N
       complex(kind=cp)                             :: Det

       !> Init
       det=cmplx(0.0,0.0)

       !> Check
       select case (n)
          case (0:1)
             Err_CFML%Ierr=1
             Err_CFML%Msg="MATHS@DETERMINANT_C: Dimension <=1 for Determinant procedure!"

          case (2)
             det=Deter2_C(A)
          case (3)
             det=Deter3_C(A)
          case (4)
             det=Deter4_C(A)
          case default
             det=DeterN_C(A,n)
       end select

    End Function Determinant_C

    !!----
    !!---- DETERMINANT_I
    !!----    Calculation of de Determinant of a integer array A(N,N)
    !!----
    Module Function Determinant_I(A,n) Result(Det)
       !---- Arguments ----!
       integer, dimension(:,:), intent(in) :: A
       integer,                 intent(in) :: N
       integer                             :: Det

       !> Init
       det=0

       !> Check
       select case (n)
          case (0:1)
             Err_CFML%Ierr=1
             Err_CFML%Msg="MATHS@DETERMINANT_I: Dimension <=1 for Determinant procedure!"

          case (2)
             det=Deter2_I(A)
          case (3)
             det=Deter3_I(A)
          case (4)
             det=Deter4_I(A)
          case default
             det=DeterN_I(A,n)
       end select

    End Function Determinant_I

    !!----
    !!---- DETERMINANT_R
    !!----    Calculation of de Determinant of a real array A(N,N)
    !!----
    Module Function Determinant_R(A,n) Result(Det)
       !---- Arguments ----!
       real(kind=cp), dimension(:,:), intent(in) :: A
       integer,                       intent(in) :: N
       real(kind=cp)                             :: Det

       !> Init
       det=0.0_cp

       !> Check
       select case (n)
          case (0:1)
             Err_CFML%Ierr=1
             Err_CFML%Msg="MATHS@DETERMINANT_R: Dimension <=1 for Determinant procedure!"

          case (2)
             det=Deter2_R(A)
          case (3)
             det=Deter3_R(A)
          case (4)
             det=Deter4_R(A)
          case default
             det=DeterN_R(A,n)
       end select

    End Function Determinant_R

    !!----
    !!---- PseudoDeterm_C
    !!----    Calculates the pseudo-determinant of a complex square matrix.
    !!----    The calculated value is only useful for linear dependency purposes.
    !!----
    !!----    It tell us if the complex matrix is singular or not.
    !!----
    !!---- 01/04/2019
    !!
    Module Function PseudoDeterm_C(A,n) Result(Det)
       !---- Arguments ----!
       complex(kind=cp), dimension(:,:), intent( in) :: A         ! input square matrix (n,n)
       integer,                          intent( in) :: n         ! actual dimension of A
       real(kind=cp)                                 :: det       ! det(AR)^2 + det(AI)^2

       !---- local variables ----!
       real(kind=cp),    dimension(2*n,2*n) :: AC   !real square matrix
       real(kind=cp)                        :: d
       integer                              :: i,nn
       logical                              :: singular

       nn=2*n
       AC(  1:n ,  1:n ) =  real(A(1:n ,1:n))
       AC(n+1:nn,  1:n ) = aimag(A(1:n ,1:n))
       AC(n+1:nn,n+1:nn) =    AC(  1:n ,1:n)
       AC(  1:n ,n+1:nn) =   -AC(n+1:nn,1:n)

       Det=0.0_cp
       call lu_decomp(ac(1:nn,1:nn),d,singular)

       if (singular) then
          err_cfml%ierr=1
          err_cfml%msg="MATHS@PSEUDODETERM: The array is singular!"

       else
          do i=1,nn
             d=d*sign(1.0_cp,ac(i,i))
             det=det + log(abs(ac(i,i)))
          end do
          det=d*exp(det)
       end if

    End Function PseudoDeterm_C

    !!----
    !!---- DETER2_C
    !!----    Performs a direct calculation of the determinant of a 2×2 matrix
    !!----
    !!---- 02/04/2019
    !!
    Pure Module Function Deter2_C(A) Result(Det)
       !---- arguments ----!
       complex(kind=cp), dimension(2,2), intent(in) :: A   !! Matrix
       complex(kind=cp)                             :: Det      !! Determinant

       !> Calculate the inverse determinant of the matrix
       det = A(1,1)*A(2,2) - A(1,2)*A(2,1)

    End Function Deter2_C

    !!----
    !!---- DETER2_I
    !!----    Performs a direct calculation of the determinant of a 2×2 matrix
    !!----
    !!---- 02/04/2019
    !!
    Pure Module Function Deter2_I(A) Result(Det)
       !---- arguments ----!
       integer, dimension(2,2), intent(in) :: A  !! Matrix
       integer                             :: Det      !! Determinant

       !> Calculate the inverse determinant of the matrix
       det = A(1,1)*A(2,2) - A(1,2)*A(2,1)

    End Function Deter2_I

    !!----
    !!---- DETER2_R
    !!----    Performs a direct calculation of the determinant of a 2×2 matrix
    !!----
    !!---- 02/04/2019
    !!
    Pure Module Function Deter2_R(A) Result(Det)
       !---- arguments ----!
       real(kind=cp), dimension(2,2), intent(in) :: A   !! Matrix
       real(kind=cp)                             :: Det      !! Determinant

       !> Calculate the inverse determinant of the matrix
       det = A(1,1)*A(2,2) - A(1,2)*A(2,1)

    End Function Deter2_R

    !!----
    !!---- DETER3_C
    !!----    Performs a direct calculation of the determinant of a 3x3 matrix
    !!----
    !!---- 02/04/2019
    !!
    Pure Module Function Deter3_C(A) Result(Det)
       !---- arguments ----!
       complex(kind=cp), dimension(3,3), intent(in) :: A   !! Matrix
       complex(kind=cp)                             :: Det      !! Determinant

       !> Calculate the inverse determinant of the matrix
       det = A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2) &
           - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1) &
           + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1)

    End Function Deter3_C

    !!----
    !!---- DETER3_I
    !!----    Performs a direct calculation of the determinant of a 3x3 matrix
    !!----
    !!---- 02/04/2019
    !!
    Pure Module Function Deter3_I(A) Result(Det)
       !---- arguments ----!
       integer, dimension(3,3), intent(in) :: A   !! Matrix
       integer                             :: Det      !! Determinant

       !> Calculate the inverse determinant of the matrix
       det = A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2) &
           - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1) &
           + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1)

    End Function Deter3_I

    !!----
    !!---- DETER3_R
    !!----    Performs a direct calculation of the determinant of a 3x3 matrix
    !!----
    !!---- 02/04/2019
    !!
    Pure Module Function Deter3_R(A) Result(Det)
       !---- arguments ----!
       real(kind=cp), dimension(3,3), intent(in) :: A   !! Matrix
       real(kind=cp)                             :: Det      !! Determinant

       !> Calculate the determinant of the matrix
       det = A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2) &
           - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1) &
           + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1)

    End Function Deter3_R

    !!----
    !!---- DETER4_C
    !!----    Performs a direct calculation of the determinant of a 4x4 matrix
    !!----
    !!---- 02/04/2019
    !!
    Pure Module Function Deter4_C(A) Result(Det)
       !---- arguments ----!
       complex(kind=cp), dimension(4,4), intent(in) :: A   !! Matrix
       complex(kind=cp)                             :: Det      !! Determinant

       !> Calculate the determinant of the matrix
       det = A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+ &
             A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))) &
           - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+ &
             A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))) &
           + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+ &
             A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))) &
           - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+ &
             A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))

    End Function Deter4_C

    !!----
    !!---- DETER4_I
    !!----    Performs a direct calculation of the determinant of a 4x4 matrix
    !!----
    !!---- 02/04/2019
    !!
    Pure Module Function Deter4_I(A) Result(Det)
       !---- arguments ----!
       integer, dimension(4,4), intent(in) :: A   !! Matrix
       integer                             :: Det      !! Determinant

       !> Calculate the determinant of the matrix
       det = A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+ &
             A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))) &
           - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+ &
             A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))) &
           + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+ &
             A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))) &
           - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+ &
             A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))

    End Function Deter4_I

    !!----
    !!---- DETER4_R
    !!----    Performs a direct calculation of the determinant of a 4x4 matrix
    !!----
    !!---- 02/04/2019
    !!
    Pure Module Function Deter4_R(A) Result(Det)
       !---- arguments ----!
       real(kind=cp), dimension(4,4), intent(in) :: A   !! Matrix
       real(kind=cp)                             :: Det      !! Determinant

       !> Calculate the determinant of the matrix
       det = A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+ &
             A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))) &
           - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+ &
             A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))) &
           + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+ &
             A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))) &
           - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+ &
             A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))

    End Function Deter4_R

    !!----
    !!---- DETERN_C
    !!----    Calculation of de Determinant of a complex array A(N,N)
    !!----
    Module Function DeterN_C(A,n) Result(Det)
       !---- Arguments ----!
       complex(kind=cp), dimension(:,:), intent(in) :: A
       integer,                          intent(in) :: N
       complex(kind=cp)                             :: Det

       !---- Local Variables ----!
       complex(kind=cp), dimension(n,n) :: aa
       complex(kind=cp), dimension(n)   :: cs
       complex(kind=cp)                 :: pv, tt
       real(kind=cp)                    :: pav
       integer,          dimension(n)   :: pc,pl
       integer                          :: i,j,k,ik,jk

       !> Init
       det=cmplx(1.0,0.0)
       pc=0; pl=0
       cs=cmplx(0.0,0.0)
       aa=A(1:n,1:n)

       do k=1,n
          !> Searching greatest pivot
          pv=aa(k,k)
          ik=k
          jk=k
          pav=abs(pv)

          do i=k,n
             do j=k,n
                if (abs(aa(i,j)) > pav) then
                   pv=aa(i,j)
                   pav=abs(pv)
                   ik=i
                   jk=j
                end if
             end do
          end do

          !> Search terminated, the pivot is in location I=IK, J=JK.
          !> Memorizing pivot location:
          pc(k)=jk
          pl(k)=ik

          !> Determinant DET is actualised
          !>If DET=0, ERROR MESSAGE and STOP
          if (ik /= k) det=-det
          if (jk /= k) det=-det
          det=det*pv
          if (abs(det) < epsilon(1.0_cp)) return

          !> positionning pivot in k,k
          if (ik /= k) then
             do i=1,n
                !> exchange lines ik and k
                tt=aa(ik,i)
                aa(ik,i)=aa(k,i)
                aa(k,i)=tt
             end do
          end if

          !> Pivot is at correct line
          if (jk /= k) then
             do i=1,n
                !> exchange columns jk and k of matrix aa
                tt=aa(i,jk)
                aa(i,jk)=aa(i,k)
                aa(i,k)=tt
             end do
          end if

          !> Pivot is at correct column and located in K,K
          !> Store column K in vector CS then set column K to zero
          do i=1,n
             cs(i)=aa(i,k)
             aa(i,k)=cmplx(0.0,0.0)
          end do

          cs(k)=cmplx(0.0,0.0)
          aa(k,k)=cmplx(1.0,0.0)

          !> Modify line K :
          if (abs(pv) < epsilon(1.0_cp)) then
             !> Error
             Err_CFML%Ierr=1
             Err_CFML%Msg="MATHS@DETERN_C: Pivot too small for this complex array!"
             return
          end if

          do i=1,n
             aa(k,i)=aa(k,i)/pv
          end do

          !> Modify other lines of matrix AA:
          do j=1,n
             if (j == k) continue
             do i=1,n
                !> modify line j of matrix aa
                aa(j,i)=aa(j,i)-cs(j)*aa(k,i)
             end do
          end do

          !> End of K loop
       end do

    End Function DeterN_C

    !!----
    !!---- DETERN_I
    !!----    Calculates the determinant of an integer square matrix.
    !!----
    !!---- 01/04/2019
    !!
    Module Function DeterN_I(A,n) Result(Det)
       !---- Arguments ----!
       integer, dimension(:,:), intent( in) :: A      ! Input array NxN
       integer,                 intent( in) :: n      ! Dimension of A
       integer                              :: Det    ! Value

       !---- local variables ----!
       real(kind=cp),    dimension(n,n)  :: AC
       real(kind=cp)                     :: d,dd
       integer                           :: i
       logical                           :: singular

       !> Init
       det=0

       !> Copy
       ac=real(A(1:n,1:n))

       !> Descomposition
       call lu_decomp(ac,d,singular)
       if (singular) then
          err_cfml%Ierr=1
          err_cfml%msg="MATHS@DETERN_I:The array is singular!"
          return
       end if

       dd=0.0_cp
       do i=1,n
          d=d*sign(1.0_cp,ac(i,i))
          dd=dd + log(abs(ac(i,i)))
       end do
       dd=d*exp(dd)
       det=nint(dd)

    End Function DeterN_I

    !!----
    !!---- DETERN_R
    !!----    Calculates the determinant of a real square matrix.
    !!----
    !!---- 01/04/2019
    !!
    Module Function DeterN_R(A,n) Result(Det)
       !---- Arguments ----!
       real(kind=cp), dimension(:,:), intent( in) :: A      ! Input array NxN
       integer,                       intent( in) :: n      ! Dimension of A
       real(kind=cp)                              :: Det    ! Value

       !---- local variables ----!
       real(kind=cp),    dimension(n,n)  :: AC
       real(kind=cp)                     :: d
       integer                           :: i
       logical                           :: singular

       !> Init
       det=0.0_cp

       !> Copy
       ac=A(1:n,1:n)

       !> Descomposition
       call lu_decomp(ac,d,singular)
       if (singular) then
          err_cfml%Ierr=1
          err_cfml%msg="MATHS@DETERN_R:The array is singular!"
          return
       end if

       do i=1,n
          d=d*sign(1.0_cp,ac(i,i))
          det=det + log(abs(ac(i,i)))
       end do
       det=d*exp(det)

    End Function DeterN_R

    !!----
    !!---- DETERM_V_I
    !!----    Calculates the determinant of the components of three vectors
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Determ_V_I(Vec1,Vec2,Vec3) Result(det)
       !---- Arguments ----!
       integer, dimension(3), intent(in) :: Vec1,Vec2,Vec3
       integer                           :: det

       !---- Local variables ----!
       integer :: i,j,k

       det = 0
       do i = 1,3
          j = i+1
          if (j == 4) j = 1
          k = 6-i-j
          det = det+Vec1(i)*(Vec2(j)*Vec3(k)-Vec2(k)*Vec3(j))
       end do

    End Function Determ_V_I

    !!----
    !!---- DETERM_V_R
    !!----    Calculates the determinant of the components of three vectors
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Determ_V_R(Vec1,Vec2,Vec3) Result(det)
       !---- Arguments ----!
       real(kind=cp), dimension(3), intent(in) :: Vec1,Vec2,Vec3
       real(kind=cp)                           :: det

       !---- Local variables ----!
       integer :: i,j,k

       det = 0.0_cp
       do i = 1,3
          j = i+1
          if (j == 4) j = 1
          k = 6-i-j
          det = det+Vec1(i)*(Vec2(j)*Vec3(k)-Vec2(k)*Vec3(j))
       end do

    End Function Determ_V_R

End Submodule Math_Determinant
