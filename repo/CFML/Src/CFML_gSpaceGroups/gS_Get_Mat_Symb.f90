!!----
!!----
!!----
!!
SubModule (CFML_gSpaceGroups) gS_Get_Mat_Symb
   implicit none
   Contains

   !!----
   !!---- Get_Mat_From_Symb_Int
   !!----
   !!----  This subroutine provides the rational matrix Mat in standard
   !!----  form (all translation components positive) corresponding to the
   !!----  operator symbol Symb in Jone's faithful notation.
   !!----
   !!----  The symbol is not modified but the matrix contain reduced translation.
   !!----  Some checking on the correctness of the symbol is performed
   !!----
   !!----  If symb represent orientation changes (ba-c for example) the symb argument
   !!----  have to be written as: b,a,-c;0,0,0. The output matrix P in this case
   !!----  corresponds to the International convention: (a',b',c',..)=(a,b,c,..) P
   !!----
   !!---- 20/04/2019
   !!---- 05/02/2020 Transposition of the rotational part of Mat to transform it to P
   !!
   Module Subroutine Get_Mat_From_Symb_Int(Symb, Mat, Invt)
      !---- Arguments ----!
      character(len=*),                intent(in)  :: Symb
      type(rational), dimension(:,:),  intent(out) :: Mat
      integer, optional,               intent(out) :: invt

      !---- local variables ----!
      integer                 :: i,j,k,Dd, d, np, n,m,inv,num,den,ind,ier
      integer, dimension(40)  :: pos,pn

      character(len=len(Symb))                                     :: string, pSymb, translation
      character(len=3),dimension(10)                               :: x_typ
      character(len=len(Symb)), dimension(size(Mat,dim=1))         :: split
      character(len=10),        dimension(size(Mat,dim=1))         :: subst
      character(len=80),dimension(size(Mat,dim=1),size(Mat,dim=1)) :: matrix
      character(len=5)                                             :: forma

      type(rational) :: det
      logical        :: abc_transf

      !> Init
      call clear_error()

      Dd=size(Mat,dim=1)
      d=Dd-1
      abc_transf=.false.
      x_typ=xyz
      i=index(Symb,"x2")
      if (i /= 0) x_typ=x1x2x3

      i=index(Symb,"b")
      if (i /= 0) then
         x_typ=abc
         abc_transf=.true.
      else if(index(Symb,"a1") /= 0) then
         x_typ=A1A2A3
         abc_transf=.true.
      end if

      Mat=0//1
      if (abc_transf) then
         i=index(Symb,";")
         if (i == 0) then
            Err_CFML%Ierr=1
            Err_CFML%flag=.true.
            Err_CFML%Msg="Get_Mat_From_Symb_Int@SPACEG: Error in a,b,c,..;0,0,... notation: "//trim(Symb)
            return
         end if
         translation=pack_string(Symb(i+1:)//",0")
         pSymb=pack_string(Symb(1:i-1)//",1")
         call Get_Separator_Pos(translation,",",pos,np)
         if (np /= d) then
            Err_CFML%Ierr=1
            Err_CFML%flag=.true.
            Err_CFML%Msg="Get_Mat_From_Symb_Int@SPACEG: Error transformation symbol: "//trim(Symb)
            return
         end if

         j=1
         do i=1,d
            split(i)=translation(j:pos(i)-1)
            j=pos(i)+1
         end do

         do i=1,d
            k=index(split(i),"/")
            if (k /= 0) then
               read(unit=split(i)(1:k-1),fmt=*) num
               read(unit=split(i)(k+1:),fmt=*) den
            else
               den=1
               read(unit=split(i),fmt=*) num
            end if
            Mat(i,Dd)= num//den
         end do
         split=" "

      else
         pSymb=pack_string(Symb)
      end if

      if (index(pSymb,"=") /= 0) then
         Err_CFML%Ierr=1
         Err_CFML%Msg="Get_Mat_From_Symb_Int@SPACEG: Error in the symbol of the operator: symbol '=' is forbidden!"
         return
      end if

      if (index(pSymb,";") /= 0) then
         Err_CFML%Ierr=1
         Err_CFML%flag=.true.
         Err_CFML%Msg="Get_Mat_From_Symb_Int@SPACEG: Error in the symbol of the operator: symbol ';' is forbidden!"
         return
      end if

      if (index(pSymb,".") /= 0) then
         Err_CFML%Ierr=1
         Err_CFML%flag=.true.
         Err_CFML%Msg="Get_Mat_From_Symb_Int@SPACEG: "// &
                      "Error in the symbol of the operator: symbol '.' is forbidden!"
         return
      end if

      pos=0
      call Get_Separator_Pos(pSymb,",",pos,np)

      if (np /= d) then
         if (np == d-1) then
            do i=1,d
               j=index(pSymb,trim(x_typ(i)))
               if (j == 0) then !error in the symbol
                  Err_CFML%Ierr=1
                  Err_CFML%flag=.true.
                  Err_CFML%Msg="Get_Mat_From_Symb_Int@SPACEG: "// &
                               "Error in the symbol of the operator: Missing ( " // &
                               trim(x_typ(i))//" ) => Symbol:"//trim(pSymb)
                  return
               end if
            end do
            pSymb=trim(pSymb)//",1"
            np=np+1
            pos(np)=len_trim(pSymb)-1
         else
            Err_CFML%Ierr=1
            Err_CFML%flag=.true.
            write(unit=Err_CFML%Msg, fmt="(a,2i3,a)") "Get_Mat_From_Symb_Int@SPACEG: "// &
                 "Error in the dimension of the symbol operator: "//trim(pSymb), np, d, &
                 " for n_commas and dimension"
            return
         end if

      else !Check the presence of all symbols
         do i=1,d
            j=index(pSymb(1:pos(np)),trim(x_typ(i)))
            if (j == 0) then !error in the symbol
               Err_CFML%Ierr=1
               Err_CFML%flag=.true.
               Err_CFML%Msg="Get_Mat_From_Symb_Int_Op@SPACEG: "// &
                            "Error in the symbol of the operator: Missing ( "//trim(x_typ(i))// &
                            " ) => Symbol:"//trim(pSymb)
               return
            end if
         end do
      end if

      read(unit=pSymb(pos(np)+1:),fmt=*,iostat=ier) inv
      if (ier == 0) then
         Mat(Dd,Dd)=1//1
         if (present(invt)) invt = inv
      else
         Mat(Dd,Dd)=1//1
         if (present(invt)) invt = 1
      end if
      j=1
      do i=1,d
         split(i)=pSymb(j:pos(i)-1)
         j=pos(i)+1
      end do

      !> Analysis of each item
                      !   |     |  |  |  <- pos(i)
      !items of the form  -x1+3/2x2-x3+x4+1/2
                      !   |  |     |  |  |  <- pn(m)
                      !   -  +3/2  -  +  +1/2
      do i=1,d
         string=split(i)
         pn=0; m=0
         do j=1,len_trim(string)
            if (string(j:j) == "+" .or. string(j:j) == "-" ) then
               m=m+1
               pn(m)=j
            end if
         end do
         if (m /= 0) then
            if (pn(1) == 1) then
               n=0
            else
               subst(1)=string(1:pn(1)-1)
               n=1
            end if
            do k=1,m-1
               n=n+1
               subst(n)=string(pn(k):pn(k+1)-1)
            end do
            if (pn(m) < len_trim(string)) then
               n=n+1
               subst(n)=string(pn(m):)
            end if
         else
            n=1
            subst(1)=string
         end if

         !> Now we have n substrings of the form +/-p/qXn  or +/-p/q or +/-pXn/q or Xn or -Xn
         !> Look for each substring the item x_typ and replace it by 1 or nothing
         !> m is reusable now
         do j=1,n
            k=0
            do m=1,d
               k=index(subst(j),trim(x_typ(m)))
               if (k /= 0) then
                  ind=m
                  exit
               end if
            end do

            if (k == 0) then !pure translation
               k=index(subst(j),"/")
               if (k /= 0) then
                  read(unit=subst(j)(1:k-1),fmt=*) num
                  read(unit=subst(j)(k+1:),fmt=*) den
               else
                  den=1
                  read(unit=subst(j),fmt=*) num
               end if
               Mat(i,Dd)= num//den

            else  !Component m of the row_vector
               !> suppress the symbol
               subst(j)(k:k+len_trim(x_typ(ind))-1)=" "
               if ( k == 1 ) then
                  subst(j)(1:1)="1"
               else if(subst(j)(k-1:k-1) == "+" .or. subst(j)(k-1:k-1) == "-") then
                  subst(j)(k:k)="1"
               else
                  subst(j)=pack_string(subst(j))
               end if

               !> Now read the integer or the rational
               k=index(subst(j),"/")
               if (k /= 0) then
                  read(unit=subst(j)(1:k-1),fmt=*) num
                  read(unit=subst(j)(k+1:),fmt=*) den
               else
                  den=1
                  read(unit=subst(j),fmt=*) num
               end if
               Mat(i,ind)=num//den
            end if
         end do
      end do

      if(abc_transf) then
        !Put the transformation matrix in International Convention
        Mat(1:d,1:d)=transpose(Mat(1:d,1:d))
      else
        !> Put the operator in standard form with positive translations
        call reduced_translation(Mat)
      end if

      !> Final check that the determinant of the rotational matrix is integer
      !det=rdet(Mat)
      det=Rational_Determ(Mat)

      if (det%numerator == 0) then
         Err_CFML%Ierr=1
         Err_CFML%flag=.true.
         Err_CFML%Msg="Get_Mat_From_Symb_Int@SPACEG: "// &
                      "The matrix of the operator is singular! -> det="//Rational_String(det)
         matrix=Rational_String(Mat)

         forma="( a8)"
         write(unit=forma(2:2),fmt="(i1)") Dd
         do j=1,Dd
            write(unit=*,fmt=forma) (trim( Matrix(j,k))//" ",k=1,Dd)
         end do
      end if
   End Subroutine Get_Mat_From_Symb_Int

End SubModule gS_Get_Mat_Symb

