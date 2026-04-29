!!----
!!----
!!----
!!
SubModule (CFML_gSpaceGroups) gS_Get_Symb_Oper
   implicit none
   !> Local Variable
   real(kind=cp), parameter :: LEPS=0.0002_cp

   Contains

   !!----
   !!---- STRING_FROM_OP
   !!----
   !!---- 19/04/2019
   !!
   Module Function String_from_Op(Op, Strcode) Result(symb)
      !---- Arguments ----!
      type(Symm_Oper_Type),       intent(in) :: Op
      character(len=*), optional, intent(in) :: Strcode
      character(len=:), allocatable          :: symb

      !> Init
      symb=" "
      if (present(strcode)) then
         Symb=Get_Symb_from_Mat(Op%Mat,strcode,Op%time_inv)
      else
         Symb=Get_Symb_from_Mat(Op%Mat,"xyz",Op%time_inv)
      end if
   End Function String_from_Op

   !!----
   !!----  STRING_FROM_MAT_TR_R
   !!----     Returning a string for symmetry operators or for points,
   !!----     axes or plane give as written in fractional form
   !!----
   !!---- 15/05/2019
   !!
   Module Function String_from_MAT_TR_R(Mat,T) Result(Symb)
      !---- Arguments ----!
      real(kind=cp),    dimension(3,3), intent( in) :: Mat
      real(kind=cp),    dimension(3),   intent( in) :: t
      character(len=:), allocatable                 :: symb

      !---- Local Variables ----!
      character(len= 30)       :: car
      integer                  :: i,j,k, np,npp,npos
      real(kind=cp)            :: suma

      !> Init
      symb=" "

      npos=1
      do i=1,3
         npp=0
         do j=1,3
            if (abs(mat(i,j)) > 0.0_cp ) then
               car=String_Fraction_2Dig(mat(i,j))
               car=adjustl(car)
               if (abs(abs(mat(i,j))-1.0_cp) <= LEPS) then
                  if (npp == 0) then
                     select case (car(1:2))
                        case ("-1")
                           car(2:)=car(3:)//"  "
                         case ("+1")
                           car=car(3:)//"  "
                     end select
                  else
                     car(2:)=car(3:)//"  "
                  end if
               else
                  if (npp == 0) then
                     if (car(1:1) =="+") then
                        car=car(2:)//"  "
                     end if
                  end if
               end if

               np=len_trim(car)
               select case (j)
                  case (1)
                     k=index(car(1:np),"/")
                     if ( k /= 0) then
                        if (car(k-1:k-1) == "1") then
                           car(k-1:k-1) = "x"
                           symb(npos:)=car(1:np)
                        else
                           symb(npos:)=car(1:k-1)//"x"//car(k:np)
                        end if
                     else
                        symb(npos:)=car(1:np)//"x"
                     end if

                  case (2)
                     k=index(car(1:np),"/")
                     if ( k /= 0) then
                        if (car(k-1:k-1) == "1") then
                           car(k-1:k-1) = "y"
                           symb(npos:)=car(1:np)
                        else
                           symb(npos:)=car(1:k-1)//"y"//car(k:np)
                        end if

                     else
                        symb(npos:)=car(1:np)//"y"
                     end if

                  case (3)
                     k=index(car(1:np),"/")
                     if ( k /= 0) then
                        if (car(k-1:k-1) == "1") then
                           car(k-1:k-1) = "z"
                           symb(npos:)=car(1:np)
                        else
                           symb(npos:)=car(1:k-1)//"z"//car(k:np)
                        end if

                     else
                        symb(npos:)=car(1:np)//"z"
                     end if
               end select
               npos=len_trim(symb)+1
               npp=npos
            end if
         end do

         if (abs(t(i)) <= LEPS .and. npp /= 0) then
            if (i < 3) then
               symb(npos:)=", "
               npos=len_trim(symb)+2
            end if
            cycle
         end if

         car=String_Fraction_2Dig(t(i))
         car=adjustl(car)
         suma=0.0_cp
         do j=1,3
            suma=suma+abs(mat(i,j))
         end do
         np=len_trim(car)
         if (suma <= 3.0_cp*LEPS) then
            if (car(1:1) == "+") car=car(2:np)//" "
         end if

         if (i < 3) then
            symb(npos:)=car(1:np)//", "
            npos=len_trim(symb)+2
         else
            symb(npos:)=car(1:np)
         end if
      end do

      symb=pack_string(symb)

   End Function String_from_MAT_TR_R

   !!----
   !!---- STRING_FROM_MAT_TR_I
   !!----    Obtain the Jones Faithful representation of a symmetry operator
   !!----
   !!---- 15/05/2019
   !!
   Module Function String_from_MAT_TR_I(MAT,T) Result(Symb)
      !---- Arguments ----!
      integer,       dimension(3,3), intent( in) :: Mat
      real(kind=cp), dimension(3),   intent( in) :: T
      character(len=:), allocatable              :: Symb

      !---- Local Variables ----!
      character(len=*),dimension(3),parameter :: xyz=["x","y","z"]
      character(len= 30)              :: car
      character(len= 30),dimension(3) :: sym
      integer                         :: i,j

      !> Init
      symb=" "

      do i=1,3
         sym(i)=" "
         do j=1,3
            if (mat(i,j) == 1) then
               sym(i) = trim(sym(i))//"+"//xyz(j)
            else if(mat(i,j) == -1) then
               sym(i) =  trim(sym(i))//"-"//xyz(j)
            else if(mat(i,j) /= 0) then
               car=" "
               write(unit=car,fmt="(i3,a)") mat(i,j),xyz(j)
               if (mat(i,j) > 0) car="+"//trim(car)
               sym(i)=trim(sym(i))//pack_string(car)
            end if
         end do

         if (abs(t(i)) > LEPS ) then
            car=String_Fraction_2Dig(t(i))
            sym(i)=trim(sym(i))//trim(car)
         end if
         sym(i)=adjustl(sym(i))
         if (sym(i)(1:1) == "+")  then
            sym(i)(1:1) = " "
            sym(i)=adjustl(sym(i))
         end if
         sym(i)=pack_string(sym(i))
      end do
      symb=trim(sym(1))//","//trim(sym(2))//","//trim(sym(3))
   End Function String_from_MAT_TR_I

End SubModule gS_Get_Symb_Oper

