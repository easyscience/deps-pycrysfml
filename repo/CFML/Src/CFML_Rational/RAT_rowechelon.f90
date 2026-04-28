!!----
!!----
!!----
!!----
!!
SubModule (CFML_Rational) RAT_rowechelon
 implicit none
 Contains
   !!----
   !!---- RATIONAL_ROWECHELONFORM_M
   !!----
   !!---- 08/04/2019
   !!
   Pure Module Subroutine Rational_RowEchelonForm_M(M)
      !---- Arguments ----!
      type(rational), dimension(:,:), intent(inout) :: M

      !---- Local variables ----!
      integer                                   :: r,c,i,j
      integer                                   :: nrow,ncolumn
      logical                                   :: cleared
      type(rational)                            :: a
      type(rational), dimension(:), allocatable :: row

      nrow    = size(M,1)
      ncolumn = size(M,2)
      allocate(row(ncolumn))

      r = 1  ! index for rows
      c = 1  ! index for columns
      do
         if (r > nrow .or. c > ncolumn) exit
         i = r
         do
            if (i > nrow) exit
            if (M(i,c) /= (0//1)) exit
            i = i + 1
         end do

         if (i <= nrow ) then
            row(:) = M(r,:)
            M(r,:) = M(i,:)
            M(i,:) = row(:)
            do j = i + 1 , nrow
               a = abs(M(j,c))
               if ( a /= (0//1) .and. a < abs(M(r,c)) ) then
                  row(:) = M(r,:)
                  M(r,:) = M(j,:)
                  M(j,:) = row(:)
               end if
            end do

            if ( M(r,c) < (0//1) ) M(r,:) = -M(r,:)

            cleared = .true.
            do i = r + 1 , nrow
               a = M(i,c)/M(r,c)
               if ( a /= (0//1) ) M(i,:) = M(i,:) - a * M(r,:)
               if ( M(i,c) /= (0//1) ) cleared = .false.
            end do
            if ( cleared ) then
               r = r + 1
               c = c + 1
            end if
         else
            c = c + 1
         end if
      end do

   End subroutine Rational_RowEchelonForm_M

   !!----
   !!---- RATIONAL_ROWECHELONFORM_MT
   !!----
   !!---- 08/04/2019
   !!
   Pure Module Subroutine Rational_RowEchelonForm_MT(M,T)
      !---- Arguments ----!
      type(rational), dimension(:,:), intent(inout) :: M
      type(rational), dimension(:,:), intent(inout) :: T

      !---- Local variables ----!
      integer                                   :: r,c,i,j
      integer                                   :: nrow,ncolumn
      logical                                   :: cleared
      type(rational)                            :: a
      type(rational), dimension(:), allocatable :: row, Trow

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
            if (i > nrow) exit
            if (M(i,c) /= (0//1)) exit
            i = i + 1
         end do

         if ( i <= nrow ) then
            row(:)  = M(r,:)
            M(r,:)  = M(i,:)
            M(i,:)  = row(:)
            Trow(:) = T(r,:)
            T(r,:)  = T(i,:)
            T(i,:)  = Trow(:)
            do j = i + 1 , nrow
               a = abs(M(j,c))
               if ( a /= (0//1) .and. a < abs(M(r,c)) ) then
                  row(:)  = M(r,:)
                  M(r,:)  = M(j,:)
                  M(j,:)  = row(:)
                  Trow(:) = T(r,:)
                  T(r,:)  = T(j,:)
                  T(j,:)  = Trow(:)
               end if
            end do

            if ( M(r,c) < (0//1) ) then
               M(r,:) = - M(r,:)
               T(r,:) = - T(r,:)
            end if

            cleared = .true.
            do i = r + 1 , nrow
                a = M(i,c)/M(r,c)
                if ( a /= (0//1) ) then
                   M(i,:) = M(i,:) - a * M(r,:)
                   T(i,:) = T(i,:) - a * T(r,:)
                end if
                if ( M(i,c) /= (0//1) ) cleared = .false.
            end do
            if (cleared) then
               r = r + 1
               c = c + 1
            end if
         else
            c = c + 1
         end if
      end do

   End Subroutine Rational_RowEchelonForm_MT

   !!----
   !!---- RATIONAL_SMITHNORMALFORM
   !!----
   !!---- 08/04/2019
   !!
   Module Subroutine Rational_SmithNormalForm(M,D,P,Q)
      !---- Arguments ----!
      type(rational), dimension(:,:), intent(in)  :: M     ! M(NR,NC)
      type(rational), dimension(:,:), intent(out) :: D     ! D(NR,NC)
      type(rational), dimension(:,:), intent(out) :: P     ! P(NR,NR)
      type(rational), dimension(:,:), intent(out) :: Q     ! Q(NC,NC)

      !---- Local variables ----!
      integer                                        :: nr,nc,ndiag
      type(rational), dimension(size(M,2),size(M,1)) :: Dt

      !> Init
      nr=size(M,1)
      nc=size(M,2)

      !> P and Q must be initialized to the identity matrix
      call Rational_Identity_Matrix(P)  ! (nr,nr)
      call Rational_Identity_Matrix(Q)  ! (nc,nc)
      D = M

      ndiag = 0
      do
         if (mod(ndiag,2) == 0) then
            call Rational_RowEchelonForm(D,P)
            ndiag = ndiag + 1
            Dt = transpose(D)
         else
            call Rational_RowEchelonForm(Dt,Q)
            ndiag = ndiag + 1
            D = transpose(Dt)
         end if
         if (Rational_Is_DiagonalMatrix(D)) exit
         if (ndiag > 100) then
            Err_CFML%IErr=1
            Err_CFML%Msg="Error in Rational_SmithNormalForm. Unable to diagonalize matrix."
            return
         end if
      end do

      Q=transpose(Q)

   End Subroutine Rational_SmithNormalForm

End SubModule RAT_rowechelon