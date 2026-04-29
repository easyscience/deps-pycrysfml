!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_RowEchelon
 implicit none
 Contains

    !!----
    !!---- ROWECHELONFORMM
    !!----
    !!---- Fortran version of RowEchelonForm from the CrystGAP package
    !!----
    !!---- The original source code can be found at:
    !!---- https://fossies.org/linux/gap/pkg/cryst/gap/common.gi
    !!----
    !!----
    Pure Module Subroutine RowEchelonFormM(M)
       !---- Arguments ----!
       integer, dimension(:,:), intent(in out) :: M

       !---- Local Variables ----!
       integer                            :: r,c,i,j,a
       integer                            :: nrow,ncolumn
       integer, dimension(:), allocatable :: row
       logical                            :: cleared

       nrow    = size(M,1)
       ncolumn = size(M,2)
       allocate(row(ncolumn))

       r = 1  ! index for rows
       c = 1  ! index for columns
       do
          if (r > nrow .or. c > ncolumn) exit
          i = r
          do
             ! if ( i > r .or. M(i,c) /= 0 ) exit
             if (i > nrow) exit
             if (M(i,c) /= 0) exit
             i = i + 1
          end do

          if (i <= nrow ) then
             row(:) = M(r,:)
             M(r,:) = M(i,:)
             M(i,:) = row(:)
             do j=i+1, nrow
                a = abs(M(j,c))
                if (a/= 0 .and. a < abs(M(r,c)) ) then
                   row(:) = M(r,:)
                   M(r,:) = M(j,:)
                   M(j,:) = row(:)
                end if
             end do
             if (M(r,c) < 0 ) M(r,:) = -1 * M(r,:)
             cleared = .true.
             do i= r + 1 , nrow
                a = M(i,c)/M(r,c)
                if ( a /= 0 ) M(i,:) = M(i,:) - a * M(r,:)
                if ( M(i,c) /= 0 ) cleared = .false.
             end do
             if (cleared) then
                r = r + 1
                c = c + 1
             end if
          else
             c = c + 1
          end if
       end do

    End Subroutine RowEchelonFormM

    !!----
    !!---- ROWECHELONFORMT
    !!----  Fortran version of RowEchelonFormT from the CrystGAP package
    !!----
    !!----  The original source code can be found at:
    !!----          https://fossies.org/linux/gap/pkg/cryst/gap/common.gi
    !!----
    Pure Module Subroutine RowEchelonFormT(M,T)
       !---- Arguments ----!
       integer, dimension(:,:), intent(in out) :: M
       integer, dimension(:,:), intent(in out) :: T

       !---- Local Variables ----!
       integer                            :: r,c,i,j,a
       integer                            :: nrow,ncolumn
       integer, dimension(:), allocatable :: row, Trow
       logical                            :: cleared

       nrow    = size(M,1)
       ncolumn = size(M,2)
       allocate(row(ncolumn))
       allocate(Trow(nrow))

       r = 1  ! index for rows
       c = 1  ! index for columns

       do
          if (r > nrow .or. c > ncolumn) exit
          i = r
          do
             ! if ( i > r .or. M(i,c) /= 0 ) exit
             if (i > nrow) exit
             if (M(i,c) /= 0) exit
             i = i + 1
          end do

          if (i <= nrow ) then
             row(:)  = M(r,:)
             M(r,:)  = M(i,:)
             M(i,:)  = row(:)
             Trow(:) = T(r,:)
             T(r,:)  = T(i,:)
             T(i,:)  = Trow(:)

             do j= i + 1 , nrow
                a = abs(M(j,c))
                if (a /= 0 .and. a < abs(M(r,c)) ) then
                   row(:)  = M(r,:)
                   M(r,:)  = M(j,:)
                   M(j,:)  = row(:)
                   Trow(:) = T(r,:)
                   T(r,:)  = T(j,:)
                   T(j,:)  = Trow(:)
                end if
             end do
             if (M(r,c) < 0 ) then
                M(r,:) = -1 * M(r,:)
                T(r,:) = -1 * T(r,:)
             end if
             cleared = .true.

             do i= r + 1 , nrow
                a = M(i,c)/M(r,c)
                if (a /= 0 ) then
                   M(i,:) = M(i,:) - a * M(r,:)
                   T(i,:) = T(i,:) - a * T(r,:)
                end if
                if ( M(i,c) /= 0 ) cleared = .false.
             end do
             if (cleared ) then
                 r = r + 1
                 c = c + 1
             end if
          else
             c = c + 1
          end if
       end do

    End Subroutine RowEchelonFormT

    !!----
    !!---- SMITHNORMALFORM
    !!----    Compute the Smith Normal Form D of matrix M: D = PMQ
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Subroutine SmithNormalForm(M,D,P,Q)
       !---- Arguments ----!
       integer, dimension(:,:), intent(in)  :: M !(nr,nc)
       integer, dimension(:,:), intent(out) :: D !(nr,nc)
       integer, dimension(:,:), intent(out) :: P !(nr,nr)
       integer, dimension(:,:), intent(out) :: Q !(nc,nc)

       !--- Local variables ---!
       integer                                 :: i, ndiag, nr, nc
       integer, dimension(size(D,2),size(D,1)) :: Dt

       nr=size(M,1)
       nc=size(M,2)

       !> P and Q must be initialized to the identity matrix
       P =  0
       Q =  0
       do i = 1 , nr
          P(i,i) = 1
       end do
       do i = 1 , nc
          Q(i,i) = 1
       end do

       D = M
       ndiag = 0
       do
          if (mod(ndiag,2) == 0) then
             call RowEchelonFormT(D,P)
             ndiag = ndiag + 1
             Dt = transpose(D)
          else
             call RowEchelonFormT(Dt,Q)
             ndiag = ndiag + 1
             D = transpose(Dt)
          end if
          if (Is_Diagonal_Matrix(D)) exit
       end do

       Q = transpose(Q)

    End Subroutine SmithNormalForm

End Submodule Maths_RowEchelon
