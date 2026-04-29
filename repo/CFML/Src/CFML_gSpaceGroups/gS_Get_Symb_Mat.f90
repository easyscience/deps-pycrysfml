!!----
!!----
!!----
!!
SubModule (CFML_gSpaceGroups) gS_Get_Symb_Mat
   implicit none
   Contains

   !!----
   !!---- GET_SYMB_FROM_RATIONAL_MAT
   !!---- 19/04/2019
   !!---- 05/02/2020 (Take the transpose of the input submatrix to get the proper
   !!---              symbol for transformation matrices of the crystal basis)
   !!
   Module Function Get_Symb_from_Rational_Mat(Matt, Strcode, Invt) Result(Symb)
      !---- Arguments ----!
      type(rational), dimension(:,:), intent(in) :: Matt
      character(len=*), optional,     intent(in) :: strcode
      integer,          optional,     intent(in) :: invt
      character(len=:), allocatable              :: symb

      !---- Local Variables ----!
      character(len=3),dimension(10)    :: x_typ
      character(len=15)                 :: car
      character(len=80)                 :: translation
      character(len=30),dimension(10)   :: sym
      integer                           :: i,j,Dd,d,k
      logical                           :: abc_type
      type(rational), dimension(size(Matt,dim=1),size(Matt,dim=2))   :: Mat

      !> Init
      Dd=size(Matt,dim=1)
      d=Dd-1
      x_typ=xyz
      abc_type=.false.
      Mat=Matt
      if (present(strcode)) then
         Select Case (trim(strcode))
            case("xyz")
               x_typ=xyz

            case("x1x2x3")
               x_typ=x1x2x3

            case("abc")
               x_typ=abc
               abc_type=.true.
               Mat(1:d,1:d)=transpose(Mat(1:d,1:d))

            case Default
               x_typ=xyz
         End Select
      end if

      !> Main
      symb=" "
      translation=" "
      do i=1, d
         sym(i)=" "
         do j=1,d
            if (Mat(i,j) == 1_LI) then
               sym(i) = trim(sym(i))//"+"//trim(x_typ(j))

            else if(Mat(i,j) == -1_LI) then
               sym(i) =  trim(sym(i))//"-"//trim(x_typ(j))

            else if(Mat(i,j) /= 0_LI) then
               car=adjustl(Rational_String(Mat(i,j)))
               k=index(car,"/")
               if (k /= 0) then
                  if (car(1:1) == "1") then
                     car=trim(x_typ(j))//car(k:)

                  else if(car(1:2) == "-1") then
                     car="-"//trim(x_typ(j))//car(k:)

                  else
                     car=car(1:k-1)//trim(x_typ(j))//car(k:)
                  end if

               else
                  car=trim(car)//trim(x_typ(j))
               end if

               if (Mat(i,j) > 0_LI) car="+"//trim(car)
               sym(i)=trim(sym(i))//pack_string(car)
            end if
         end do

         !> Write here the translational part for each component
         if (Mat(i,Dd) /= 0_LI) then
            car=adjustl(Rational_String(Mat(i,Dd)))
            if (abc_type) then
               translation=trim(translation)//","//trim(car)
            else
               if (car(1:1) == "-") then
                  sym(i)=trim(sym(i))//trim(car)
               else
                  sym(i)=trim(sym(i))//"+"//trim(car)
               end if
            end if

         else
            if (abc_type) translation=trim(translation)//",0"
         end if

         sym(i)=adjustl(sym(i))
         if (sym(i)(1:1) == "+")  then
            sym(i)(1:1) = " "
            sym(i)=adjustl(sym(i))
         end if
         sym(i)=pack_string(sym(i))
      end do

      symb=sym(1)
      do i=2,d
         symb=trim(symb)//","//trim(sym(i))
      end do

      if (abc_type)then
         symb=trim(symb)//";"//trim(translation(2:))

      else
         if (present(invt)) then
            write(unit=car,fmt="(i2)") invt !Rational_String(Mat(Dd,Dd))
            if(invt > 0) car(1:1)="+"
            symb=trim(symb)//","//trim(car)
         end if
      end if
   End Function Get_Symb_from_Rational_Mat

   !!----
   !!---- GET_SYMB_FROM_TRASFM_MAT
   !!----    Provides the short symbol for a setting change defined by
   !!----    the transfomation matrix Mat and origin given by the translation
   !!----    vector tr. For instance given the matrix:
   !!----
   !!----     1  0 -1                      a'=a-c
   !!----     0  2  0   corresponding to   b'=2b
   !!----     1  0  1                      c'=a+c
   !!----     And the change of origin given by (0.5,0.0,0.5)
   !!----     The subroutine provide the symbol: (1/2,0,1/2; a-c,2b,a+c)
   !!----     If "opposite" is provided then the output is the symbol: (a-c,2b,a+c; 1/2,0,1/2)
   !!----
   !!---- 15/05/2019
   !!
   Module Function Get_Symb_from_Mat_Tr_I(Mat, tr, opposite) Result(Str)
      !---- Arguments ----!
      integer,       dimension(3,3), intent(in) :: Mat
      real(kind=cp), dimension(3),   intent(in) :: tr
      logical, optional,             intent(in) :: opposite
      character(len=:), allocatable             :: Str

      !---- Local variables ----!
      integer           :: i
      character(len=80) :: xyz_op, transl
      character(len=12) :: Fracc

      !> Init
      xyz_op=Get_Symb_from_OP(Mat,[0.0_cp,0.0_cp,0.0_cp])

      do i=1,len_trim(xyz_op)
         if (xyz_op(i:i) == "x")  xyz_op(i:i)="a"
         if (xyz_op(i:i) == "y")  xyz_op(i:i)="b"
         if (xyz_op(i:i) == "z")  xyz_op(i:i)="c"
      end do

      transl=" "
      do i=1,3
         Fracc=String_Fraction_2Dig(tr(i))
         transl=trim(transl)//trim(Fracc)//","
      end do
      i=len_trim(transl)
      transl(i:i)=";"
      do i=1,len_trim(transl)-2
         if (transl(i:i) == "+") transl(i:i)=" "
      end do
      transl=Pack_string(transl)
      str="("//trim(transl)//" "//trim(xyz_op)//")"
      if (present(opposite)) then
         i=len_trim(transl)
         str="("//trim(xyz_op)//"; "//transl(1:i-1)//")"
      end if
   End Function Get_Symb_from_Mat_Tr_I

   !Version for real matrices
   Module Function Get_Symb_from_Mat_Tr_R(Mat, tr, opposite) Result(Str)
      !---- Arguments ----!
      real(kind=cp), dimension(3,3), intent(in) :: Mat
      real(kind=cp), dimension(3),   intent(in) :: tr
      logical, optional,             intent(in) :: opposite
      character(len=:), allocatable             :: Str

      !---- Local variables ----!
      integer           :: i
      character(len=80) :: xyz_op, transl
      character(len=12) :: Fracc

      !> Init
      xyz_op=Get_Symb_from_OP(Mat,[0.0_cp,0.0_cp,0.0_cp])

      do i=1,len_trim(xyz_op)
         if (xyz_op(i:i) == "x")  xyz_op(i:i)="a"
         if (xyz_op(i:i) == "y")  xyz_op(i:i)="b"
         if (xyz_op(i:i) == "z")  xyz_op(i:i)="c"
      end do

      transl=" "
      do i=1,3
         Fracc=String_Fraction_2Dig(tr(i))
         transl=trim(transl)//trim(Fracc)//","
      end do
      i=len_trim(transl)
      transl(i:i)=";"
      do i=1,len_trim(transl)-2
         if (transl(i:i) == "+") transl(i:i)=" "
      end do
      transl=Pack_string(transl)
      str="("//trim(transl)//" "//trim(xyz_op)//")"
      if (present(opposite)) then
         i=len_trim(transl)
         str="("//trim(xyz_op)//"; "//transl(1:i-1)//")"
      end if
   End Function Get_Symb_from_Mat_Tr_R

End SubModule gS_Get_Symb_Mat

