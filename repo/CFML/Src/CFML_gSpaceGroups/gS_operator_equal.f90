SubModule (CFML_gSpaceGroups) gS_Operator_equal
   implicit none
   Contains

   !!----
   !!---- EQUAL_SYMM_OPER
   !!----
   !!---- 19/04/2019
   !!
   Pure Module Function Equal_Symm_Oper(Op1, Op2) Result(info)
      !---- Arguments ----!
      type(Symm_Oper_Type), intent(in) :: Op1,Op2
      logical                          :: info

      !> Init
      info=.false.

      if (Op1%time_inv == Op2%time_inv) then
         if (Rational_Equal(Op1%Mat,Op2%Mat)) info=.true.
      end if
   End Function Equal_Symm_Oper

   !!----
   !!---- EQUAL_SYMM_OPER
   !!----
   !!---- 19/04/2019
   !!
   Module Function Equal_Spin_Oper(Op1, Op2) Result(info)
      !---- Arguments ----!
      type(Spin_Operator_Type), intent(in) :: Op1,Op2
      logical                              :: info

      !> Init
      info=.false.

      if (Op1%time_inv == Op2%time_inv) then
         if (Equal_Vector(Op1%tr,Op2%tr,3)) then
            if (Equal_Matrix(Op1%Rot,Op2%Rot,3)) then
                if (Equal_Matrix(Op1%UMat,Op2%UMat,3)) then
                  info=.true.
                end if
            end if
         end if
      end if
   End Function Equal_Spin_Oper

   !!----
   !!---- EQUAL_GROUP
   !!----
   !!---- 19/04/2019
   !!
   Module Function Equal_Group(Gr1, Gr2) Result(info)
      !---- Arguments ----!
      class(Spg_Type), intent(in) :: Gr1
      class(Spg_Type), intent(in) :: Gr2
      logical                    :: info

      !---- Local Vatiables ----!
      integer :: i,j
      logical :: esta

      !> Init
      info=.false.

      if (Gr1%multip /= Gr2%multip) return
      do i=2,Gr1%multip
         esta=.false.
         do j=2,Gr2%multip
            if (Gr1%Op(i) == Gr2%Op(j)) then
               esta=.true.
               exit
            end if
         end do
         if (.not. esta) return
      end do
      info=.true.
   End Function Equal_Group

   !!----
   !!---- EQUAL_GROUP
   !!----
   !!---- 19/04/2019
   !!
   Module Function Equal_Spin_Group(Gr1, Gr2) Result(info)
      !---- Arguments ----!
      class(Spin_Group_Type), intent(in) :: Gr1
      class(Spin_Group_Type), intent(in) :: Gr2
      logical                            :: info

      !---- Local Vatiables ----!
      integer :: i,j
      logical :: esta

      !> Init
      info=.false.

      if (Gr1%multip /= Gr2%multip) return
      do i=2,Gr1%multip
         esta=.false.
         do j=2,Gr2%multip
            if (Gr1%Op(i) == Gr2%Op(j)) then
               esta=.true.
               exit
            end if
         end do
         if (.not. esta) return
      end do
      info=.true.
   End Function Equal_Spin_Group

   Module Function Is_Hexa(Ng,Ss) Result(Is_Hexag)
       !---- Argument ----!
       integer, intent (in)                   :: ng
       integer, dimension(:,:,:), intent(in)  :: ss   !(3,3,48)
       logical                                :: is_Hexag

       !---- Local Variables ----!
       integer :: i

       Is_Hexag=.false.
       do i=2,ng
          if (sum(abs(ss(:,1,i))) > 1) then
             Is_hexag=.true.
             exit
          end if
          if (sum(abs(ss(:,2,i))) > 1) then
             Is_hexag=.true.
             exit
          end if
       end do

    End Function Is_Hexa

End SubModule gS_Operator_equal