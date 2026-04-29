!!----
!!---- SUBMODULE CFML_String_Utilities
!!----
!!----
!!
 Submodule (CFML_Strings) StringNum
   !---- Parameters ----!
   implicit none

 Contains
    !!----
    !!---- Function
    !!----
    !!---- Convert a fractional vector string in a real vector
    !!----
    !!---- June 2023
    !!
    Module Function Get_Vec_from_FracStr(Str) Result(V)
       !---- Arguments ----!
       character(len=*), intent(in) :: str
       real(kind=cp), dimension(3)  :: V

       !---- Local Variables ----!
       integer              :: npos
       integer, dimension(2):: pos

       V=0.0_cp
       call Get_Separator_Pos(Str,',',pos,npos)
       if (npos /= 2) then
          call set_error(1,"The string don't have a correct format to define a vector: "//trim(str))
          return
       end if
       V(1)=read_fract(str(:pos(1)-1))
       V(2)=read_fract(str(pos(1)+1:pos(2)-1))
       V(3)=read_fract(str(pos(2)+1:))

    End Function Get_Vec_from_FracStr

    !!----
    !!---- STRING_FRACTION_1DIG
    !!----
    !!----    Get a string with the most simple fraction that uses single digits
    !!----    in numerator and denominator. Used, for instance, to get a character
    !!----    representation of symmetry operators.
    !!----    If no fractional representation is found a decimal expression is produced
    !!----
    !!---- 05/04/2019
    !!
    Pure Module Function String_Fraction_1Dig(V) Result(Str)
       !---- Argument ----!
       real(kind=cp),    intent( in) :: V   !  Real value
       character(:), allocatable     :: Str !  Fracction in character form

       !---- Local variables ----!
       character(len=10)::  car
       integer          ::  numerator, denominator
       real(kind=cp)    ::  num, denom, frac

       car=" "
       if (Zbelong(v)) then
          if (v > 0.0) then
             write(unit=car, fmt="(a,i2)") "+", nint(v)
          else
             write(unit=car, fmt="(i3)") nint(v)
          end if
       else
          do numerator=1,9
             num=numerator
             do denominator=2,9
                denom=denominator
                frac=num/denom
                if (Negligible(frac-abs(v))) then
                   car="    "
                   if (v > 0.0) then
                      write(unit=car, fmt="(2(a,i1))") "+",numerator,"/",denominator
                   else
                      write(unit=car, fmt="(2(a,i1))") "-",numerator,"/",denominator
                   end if
                   car=Pack_String(car)
                   str=trim(car)
                   return
                end if
             end do
          end do
          if(v >= 0.0) then
            write(unit=car, fmt="(a,f7.3)") "+", v
          else
            write(unit=car, fmt="(f8.4)") v
          end if
       end if
       car=Pack_String(car)
       str=trim(car)

    End Function String_Fraction_1Dig

    !!----
    !!---- STRING_REAL
    !!----    Return a string of w characters containing the real value VAL
    !!----
    Pure Module Function String_Real(Val,W) Result(Str)
       !---- Arguments ----!
       real(kind=cp), intent(in)  :: val        ! value to be output
       integer,       intent(in)  :: w          ! Width
       character(len=w)           :: Str

       !---- Local Variables ----!
       character(len=4) :: carw,card
       character(len=20):: forms
       integer          :: d, ineg, j
       real(kind=cp)    :: x, xlim

       !> Initialise
       Str="  "

       !> Test for NaN
       if (ieee_is_nan(val)) then
          Str(1:w-3)=' '
          Str(w-2:w)='NaN'
          return
       end if

       !> Test for INF
       if (.not. ieee_is_finite(val)) then
          Str(1:w-3)=' '
          Str(w-2:w)='INF'
          return
       end if

       x=val+0.001_cp    ! EXTRA FOR SAFETY

       !> Check on size
       if (x > 0.0_cp) then
          xlim=10**(w-1)-1.0_cp    ! means that 99. can be written into f3.0
          ineg=0
       else
          ineg=1
          xlim=10**(w-2)-1.0_cp    ! negative, so need space for sign
          x=abs(x)
       end if

       if (x > xlim)then           ! need to write in e format
          d=w-6-ineg
          if (d < 0) d=1

          write(unit=carw,fmt='(i4)') w
          carw=adjustl(carw)
          write(unit=card,fmt='(i4)') d
          card=adjustl(card)
          forms='(E'//trim(carw)//'.'//trim(card)//')'
          write(unit=Str,fmt=trim(forms)) val
          return
       end if

       !> LOOP TO FIND SIZE OF VALUE
       ! J=1           !START WITH "0" FOR DECIMAL POINT
       j=2             ! this allows for place for sign always
       do
          x=x/10.0_cp
          j=j+1
          if (x <= 1.0_cp) exit
       end do

       !> IF(INEG .EQ. 1)J=J+1 ! reinstate if we want to only allow for neg sign, and start with J=1
       d=w-j-ineg
       if (d < 0) d=0     ! safety: should never happen

       write(unit=carw,fmt='(i4)') w
       carw=adjustl(carw)
       write(unit=card,fmt='(i4)') d
       card=adjustl(card)
       forms='(F'//trim(carw)//'.'//trim(card)//')'
       write(unit=Str,fmt=trim(forms)) val

    End Function String_Real

    !!----
    !!---- FRAC_TRANS_1DIG
    !!----    returning a string describing a
    !!----    3D translation vector written in fractional form as quotient
    !!----    of 1-digit integers with sign.
    !!----             Vector -> ( 0.25, -0.4, 0.33333)
    !!----             Str ->    "(1/4,-2/5,1/3)"
    !!----
    !!---- 05/04/2019
    !!
    Pure Module Function Frac_Trans_1Dig(Vec) Result(Str)
       !---- Argument ----!
       real(kind=cp), dimension(3), intent( in)   :: Vec  ! Vector
       character(:),allocatable                   :: Str  ! String with conversion to fractional

       !---- Local Variables ----!
       character(len=8), dimension(3)   :: Frac
       integer                           :: i,j

       !> Init
       Str="(        ,        ,        )"

       do i=1,3
          Frac(i)=String_Fraction_1Dig(vec(i))
          j=index(Frac(i),"+")
          if (j /= 0) Frac(i)(j:j) = " "
       end do

       Str(2:9)  =Frac(1)
       Str(11:18)=Frac(2)
       Str(20:27)=Frac(3)
       Str=Pack_String(Str)

    End Function Frac_Trans_1Dig

    !!----
    !!---- STRING_FRACTION_2DIG
    !!----    Get a string with the most simple fraction that uses up to two
    !!----    digits in numerator and denominator. Used, for instance, to get a
    !!----    character representation of symmetry operators.
    !!----    If no fractional representation is found a decimal expression is produced
    !!----
    !!---- 05/04/2019
    !!
    Pure Module Function String_Fraction_2Dig(V) Result(Str)
       !---- Argument ----!
       real(kind=cp),    intent( in) :: v    ! Real value
       character(:), allocatable     :: Str  ! Fraction in character form

       !---- Local variables ----!
       character(len=10)  :: car
       character(len=16)  :: formm
       real(kind=cp)      :: num, denom, frac
       integer            :: numerator, denominator

       car=" "
       if (Zbelong(v)) then
          if (v > 0.0_cp) then
             formm="(a,i3)"
             write(unit=car,fmt=formm) "+", nint(v)
          else
             formm="(i4)"
             write(unit=car,fmt=formm) nint(v)
          end if
       else
          do numerator=1,24
             num=numerator
             do denominator=2,24
                denom=denominator
                frac=num/denom
                if (Negligible(frac-abs(v))) then
                   car=" "
                   formm="(a1,i1,a1,i1)"
                   if(numerator >=10 .and. denominator <=  9) formm="(a1,i2,a1,i1)"
                   if(numerator >=10 .and. denominator >= 10) formm="(a1,i2,a1,i2)"
                   if(numerator <= 9 .and. denominator >= 10) formm="(a1,i1,a1,i2)"
                   if (v > 0.0_cp) then
                      write(unit=car,fmt=formm) "+",numerator,"/",denominator
                   else
                      write(unit=car,fmt=formm) "-",numerator,"/",denominator
                   end if
                   car=Pack_String(car)
                   str=trim(car)
                   return
                end if
             end do
          end do
          if(v > 0.0) then
              write(unit=car,fmt="(a,f9.4)") "+",v
          else
              write(unit=car,fmt="(f10.4)") v
          end if
       end if
       car=Pack_String(car)
       str=trim(car)

    End Function String_Fraction_2Dig

    !!----
    !!---- FRAC_TRANS_2DIG
    !!----    Subroutine returning a string describing a
    !!----    3D translation vector written in fractional form as quotient
    !!----    of 2-digit integers with sign.
    !!----             Vector -> ( 0.3, -0.4, -5.5)
    !!----             Str ->    "(3/10,-2/5,-11/2)"
    !!----
    !!---- 05/04/2019
    !!
    Pure Module Function Frac_Trans_2Dig(Vec) Result(Str)
       !---- Argument ----!
       real(kind=cp), dimension(3), intent(in) :: Vec   ! Vector
       character(:), allocatable               :: Str   ! String with conversion to fractional

       !---- Local Variables ----!
       character (len=10), dimension(3) :: Frac
       character (len=34)               :: strc
       integer                          :: i,j

       strc="(          ,          ,          )"
       do i=1,3
          Frac(i)=String_Fraction_2Dig(vec(i))
          j=index(Frac(i),"+")
          if (j /= 0) Frac(i)(j:j) = " "
       end do
       strc( 2:11) =Frac(1)
       strc(13:22) =Frac(2)
       strc(24:33) =Frac(3)

       Strc=Pack_String(strc)
       Str=trim(strc)

    End Function Frac_Trans_2Dig

    !!----
    !!----  GET_MAT_FROM_SYMB
    !!----
    !!----  Subroutine to extract the transformation matrix corresponding
    !!----  to a symbol of the form:  m1a+m2b+m3c,m4a+m5b+m6c,m7a+m8b+m9c
    !!----  corresponding a cell transformation or a rotational symmetry operator.
    !!----  The symbols: a,b,c are not exclusive. The last variable contains the
    !!----  equivalent ones, for instance cod=(/"u","v","w"/) or cod=(/"x","y","z"/).
    !!----  The numbers m(i) may be real or integer numbers or even fractions.
    !!----  The returned real matrix corresponds to:
    !!----                           / m1   m2   m3 \
    !!----                    Mat = |  m4   m5   m6  |
    !!----                           \ m7   m8   m9 /
    !!----  In the symbol it may appear negative sign and the order within each
    !!----  direction is irrelevant, for instantce: m2b+m1a+m3c,m6c+m5b+m4a,m9c+m8b+m7a
    !!----  is strictly equivalent to the symbol given above.
    !!----  This subroutine has been modified in order to accept data of the form:
    !!----   3a/2+b-c/4, a-3b/2,c+b/2. Now the letters may be followed by the division
    !!----  symbol. Before this modification the previous item should had be given as:
    !!----   3/2a+b-1/4c, a-3/2b,c+1/2b. Singular matrices are also accepted, for instance
    !!----  the matrix corresponding to the string: 0,a+b,0 was previously incorrect, now
    !!----  the constructed matrix is as expected:
    !!----                           / 0   0   0 \
    !!----      0,a+b,0  ->   Mat = |  1   1   0  |
    !!----                           \ 0   0   0 /
    !!----
    !!---- 05/04/2019
    !!
    Module Function Get_Mat_From_Symb(Symb,cod) Result(Mat)
       !---- Arguments ----!
       character(len=*),                intent(in)  :: Symb   ! String
       character(len=1), dimension(3),  intent(in)  :: cod    ! (/"u","v","w"/) or (/"x","y","z"/)
       real(kind=cp),dimension(3,3)                 :: Mat    ! Output

       !---- local variables ----!
       integer                                :: i,j
       character(len=len(Symb)), dimension(3) :: split

       !> Init
       Mat=0.0_cp

       i=index(Symb,",")
       j=index(Symb,",",back=.true.)
       split(1)= pack_string(Symb(1:i-1))
       split(2)= pack_string(Symb(i+1:j-1))
       split(3)= pack_string(Symb(j+1:))
       do i=1,3
          Mat(i,:)=Get_Vec_From_String(trim(split(i)), cod)
       end do

    End Function Get_Mat_From_Symb

    !!----
    !!----  GET_VEC_FROM_STRING
    !!----     Auxiliary subroutine of Get_Mat_From_Symb. This subroutine extracts
    !!----     a real vector from symbol of the form:  m1a+m2b+m3c.
    !!----
    !!---- 05/04/2019
    !!
    Module Function Get_Vec_From_String(Str,Cod) Result(Vec)
       !---- Arguments ----!
       character(len=*),                intent(in)  :: str   ! Input string
       character(len=1), dimension(3),  intent(in)  :: cod   ! Code
       real(kind=cp),dimension(3)                   :: vec   ! Vector

       !--- Local variables ---!
       integer                                 :: i,k,ns,np,nterm,m,nsp,jk,jp
       integer, dimension(3)                   :: j,pos,neg, klist
       character(len=len(str)), dimension(3)   :: split

       !> Init
       Vec=0.0_cp
       if (len_trim(str) <=0) return

       call Get_Separator_Pos(str,"+",pos,np)
       call Get_Separator_Pos(str,"-",neg,ns)
       nterm=np+ns

       !> Construct the splitted terms depending on +/- separators
       Select Case (nterm)
          Case(0)  !only 1 positive item without sign
             nsp=1
             split(1)=str

          Case(1)
             Select Case(np)
                Case(0) !A single term with a negative symbol or two terms separated by the negative symbol
                   if (neg(1) == 1) then !single term
                      nsp=1
                      split(1)=str
                   else
                      nsp=2
                      split(1)=str(1:neg(1)-1)
                      split(2)=str(neg(1):)
                   end if

                Case(1) !A single term with a positive symbol or two positive terms
                   if (pos(1) == 1) then !single term
                      nsp=1
                      split(1)=str(2:)
                   else
                      nsp=2
                      split(1)=str(1:pos(1)-1)
                      split(2)=str(pos(1)+1:)
                   end if
             End Select

          Case(2)
             Select Case(np)
                Case(0) !No positive terms then (1) -cccc -dddd or (2)xxxx - yyyy -  zzzz
                   if (neg(1) == 1) then !two negative terms (1)
                      nsp=2
                      split(1)=str(1:neg(2)-1)
                      split(2)=str(neg(2):)
                   else                  !Three terms as (2)
                      nsp=3
                      split(1)=str(1:neg(1)-1)
                      split(2)=str(neg(1):neg(2)-1)
                      split(3)=str(neg(2):)
                   end if

                Case(1) !Four options (1)+xxxx-yyyy  (2)-xxxx+yyyy  (3)xxxx+yyyyy-zzzzz  (4)xxxx-yyyy+zzzz
                   if (pos(1) == 1) then !(1)
                      nsp=2
                      split(1)=str(2:neg(1)-1)
                      split(2)=str(neg(1):)
                   else if(neg(1) == 1) then  !(2)
                      nsp=2
                      split(1)=str(1:pos(1)-1)
                      split(2)=str(pos(1)+1:)
                   else if(pos(1) < neg(1)) then !(3)
                      nsp=3
                      split(1)=str(1:pos(1)-1)
                      split(2)=str(pos(1)+1:neg(1)-1)
                      split(3)=str(neg(1):)
                   else if(pos(1) > neg(1)) then !(4)
                      nsp=3
                      split(1)=str(1:neg(1)-1)
                      split(2)=str(neg(1):pos(1)-1)
                      split(3)=str(pos(1)+1:)
                   end if

                Case(2) !Two options (1)+xxxx+yyyy  (2) xxxx+yyyy+zzzz
                   if (pos(1) == 1) then !(1)
                      nsp=2
                      split(1)=str(2:pos(2)-1)
                      split(2)=str(pos(2)+1:)
                   else   !2
                      nsp=3
                      split(1)=str(1:pos(1)-1)
                      split(2)=str(pos(1)+1:pos(2)-1)
                      split(3)=str(pos(2)+1:)
                   end if
             End Select

          Case(3)
             nsp=3
             Select Case(np)
                Case(0) !No positive terms  a single option: -xxxx - yyyy -  zzzz
                   split(1)=str(1:neg(2)-1)
                   split(2)=str(neg(2):neg(3)-1)
                   split(3)=str(neg(3):)

                Case(1) !Three options (1)+xxxx-yyyy-zzzz  (2)-xxxx+yyyy-zzzz  (3)-xxxx-yyyyy+zzzzz
                   if (pos(1) == 1) then !(1)
                      split(1)=str(2:neg(1)-1)
                      split(2)=str(neg(1):neg(2)-1)
                      split(3)=str(neg(2):)

                   else if(pos(1) <  neg(2)) then  !(2)
                      split(1)=str(1:pos(1)-1)
                      split(2)=str(pos(1)+1:neg(2)-1)
                      split(3)=str(neg(2):)

                   else if(pos(1) > neg(2)) then !(3)
                      split(1)=str(1:neg(2)-1)
                      split(2)=str(neg(2):pos(1)-1)
                      split(3)=str(pos(1)+1:)
                   end if

                Case(2) !Two options (1)+xxx+yyy-zzz  (2)-xxx+yyy+zzzz (3) +xxx-yyy+zzz
                   if (neg(1) == 1) then !(2)
                      split(1)=str(1:pos(1)-1)
                      split(2)=str(pos(1)+1:pos(2)-1)
                      split(3)=str(pos(2)+1:)

                   else if(neg(1) > pos(2)) then !(1)
                      split(1)=str(2:pos(2)-1)
                      split(2)=str(pos(2)+1:neg(1)-1)
                      split(3)=str(neg(1):)
                   else if(neg(1) < pos(2)) then !(3)
                      split(1)=str(2:neg(1)-1)
                      split(2)=str(neg(1):pos(2)-1)
                      split(3)=str(pos(2)+1:)
                   end if

                Case(3) !Single option (1)+xxx+yyy+zzz
                   split(1)=str(2:pos(2)-1)
                   split(2)=str(pos(2)+1:pos(3)-1)
                   split(3)=str(pos(3)+1:)
             End Select
       End Select

       do i=1,nsp
          split(i)=pack_string(split(i))
       end do

       vec(:) =0.0; nterm=0;  klist=0
       do m=1,nsp
          k=0
          j=0
          np=len_trim(split(m))
          do i=1,3
             j(i)=index(split(m),cod(i))
             if (j(i) /= 0) then
                k =i
                nterm=nterm+1
                klist(nterm)=i
                exit
             end if
          end do

          if ( k == 0) cycle !the component is zero
          do i=1,nterm-1
             if (k == klist(i)) then
                !> This is impossible in principle
                err_cfml%ierr=1
                err_cfml%msg=" The provided symbol is illegal: "//trim(str)
                return
             end if
          end do
          jk=j(k)
          i=jk-1
          jp=jk+1
          if (i == 0 .and. np == 1 ) then !the code is the first character replace it by "1" and read the rest of the str
             split(m)(jk:jk)="1"
          else if(i == 0) then
             if (split(m)(jp:jp) ==  "/") then
                split(m)(jk:jk)="1"
             else
                split(m)(jk:jk)=" "
             end if
          else if(split(m)(i:i) == "-") then
             if (split(m)(jp:jp) ==  "/") then
                split(m)(jk:jk)="1"
             else  !There is a number on the right
                split(m)(jk:jk)=" "
             end if
          else   !there is a number on the left, remove the symbol, compact it and read
             split(m)(jk:jk)=" "
          end if
          split(m)=pack_string(split(m))
          vec(k)=Read_Fract(split(m))
       end do

    End Function Get_Vec_From_String

    !!----
    !!----  SET_SYMB_FROM_MAT
    !!----     Function to construct a symbol of the form:
    !!----         m1a+m2b+m3c,m4a+m5b+m6c,m7a+m8b+m9c
    !!----     from a real matrix of quasi-rational numbers.
    !!----
    !!----  The symbols: a,b,c are not exclusive. The last variable contains the
    !!----  equivalent ones, for instance cod=(/"u","v","w"/) or cod=(/"x","y","z"/).
    !!----  The numbers m(i) are real numbers that are converted to fractions.
    !!----  The input real matrix corresponds to:
    !!----                           / m1   m2   m3 \
    !!----                    Mat = |  m4   m5   m6  |
    !!----                           \ m7   m8   m9 /
    !!----
    !!---- 05/04/2019
    !!
    Pure Module Function Set_Symb_From_Mat(Mat,cod) Result(Symb)
       !---- Arguments ----!
       real(kind=cp),dimension(3,3),    intent(in)  :: Mat    ! Array
       character(len=1), dimension(3),  intent(in)  :: cod    ! Codes (/"u","v","w"/) or (/"x","y","z"/)
       character(len=:), allocatable                :: Symb   ! Symbol

       !---- local variables ----!
       integer                         :: i,j,k,fin,nc,aux
       integer,           dimension(2) :: pos
       real(kind=cp),     dimension(3) :: v
       character(len=80), dimension(3) :: split,msp

       !> Init
       msp=" "
       do i=1,3
          v=Mat(i,:)
          split(i)=Frac_Trans_2Dig(v)
          split(i)=split(i)(2:len_trim(split(i))-1)
       end do

       do i=1,3
          v=Mat(i,:)
          call Get_Separator_Pos(split(i),",",pos,nc)
          if (v(2) < 0.0) then
             msp(1)=split(i)(1:pos(1)-1)
          else
             msp(1)=split(i)(1:pos(1)-1)//"  +"
          end if

          if (v(3) < 0.0) then
             msp(2)=split(i)(pos(1)+1:pos(2)-1)
          else
             msp(2)=split(i)(pos(1)+1:pos(2)-1)//"  +"
          end if
          msp(3)=split(i)(pos(2)+1:)

          do j=1,3
             if (trim(msp(j)) == '0' .or. trim(msp(j)) == '0  +') then
                msp(j)=" "
             end if
          end do

          do j=1,3
             if (len_trim(msp(j)) == 0) cycle
             k=index(msp(j),"/")
             if (k /= 0) then
                if (msp(j)(k-1:k-1) == "1") then
                   read(unit=msp(j)(1:k-1),fmt=*) aux
                   if (aux == 1 .or. aux == -1) then
                      msp(j)(k-1:k-1)=cod(j)
                   else
                      msp(j)=msp(j)(1:k-1)//cod(j)//msp(j)(k:)
                   end if
                else
                   msp(j)=msp(j)(1:k-1)//cod(j)//msp(j)(k:)
                end if
             else
                k=index(msp(j),"1")
                read(unit=msp(j),fmt=*) aux
                if (aux == 1 .or. aux == -1) then
                   msp(j)(k:k)=cod(j)
                else
                   k=index(msp(j),"+")
                   if (k /= 0) then
                      msp(j)(k-1:k-1) = cod(j)
                   else
                      msp(j)=trim(msp(j))//cod(j)
                   end if
                end if
             end if
          end do
          split(i)=Pack_String(msp(1)//msp(2)//msp(3))
          fin=len_trim(split(i))
          if (split(i)(fin:fin) == "+") split(i)(fin:fin)= " "
       end do

       Symb=Pack_String(split(1)//","//split(2)//","//split(3))
       i=index(Symb,"+-")
       if (i /= 0) Symb(i:i)=" "
       i=index(Symb,"-+")
       if (i /= 0) Symb(i+1:i+1)=" "
       Symb=Pack_String(Symb)

    End Function Set_Symb_From_Mat

    !!----
    !!----  GET_TRANSF
    !!----
    !!----  This subroutine extracts the transformation matrix and the vector
    !!----  corresponding to the change of origin from a symbol of the form:
    !!----  m1a+m2b+m3c,m4a+m5b+m6c,m7a+m8b+m9c;t1,t2,t3.
    !!----  The order may be matrix;origin or origin;matrix. Parenthesis may
    !!----  accompany the symbol like in (a,b+c,c-b;1/2,0,1/2). The basis vectors
    !!----  a,b,c and the separator ";" may be changed by putting them into the
    !!----  optional array cod. For instance if cod=["u","v","w","|"] a sort of
    !!----  Seitz symbol may be read.
    !!----
    !!----  Created: January 2014 (JRC)
    !!----
    Module Subroutine Get_Transf(str,mat,v,cod)
       !---- Arguments ----!
       character(len=*),                          intent(in)  :: str      ! Input string
       real(kind=cp),dimension(3,3),              intent(out) :: mat      ! Matrix
       real(kind=cp),dimension(3),     optional,  intent(out) :: v        ! Vector
       character(len=1), dimension(4), optional,  intent(in)  :: cod      ! Code

       !--- Local variables ---!
       character(len=1), dimension(4) :: cd
       character(len=len(str))        :: transf_key,cmat,ori
       integer                        :: i,j,nc
       integer,dimension(2)           :: pos

       !> init
       mat=0.0_cp
       cd=(/"a","b","c",";"/)
       if (present(cod)) cd=cod
       transf_key=str

       !> Remove the parenthesis is present
       j=index(transf_key,"(")
       if (j /= 0) transf_key(j:j)= " "
       j=index(transf_key,")")
       if (j /= 0) transf_key(j:j)= " "
       transf_key=adjustl(l_case(transf_key))

       !> Determine the order in which the string is provided
       i=index(transf_key,cd(4))
       if (i /= 0) then
          cmat=transf_key(1:i-1)
          j=index(cmat,cd(1))
          if (j == 0) then
             ori=cmat
             cmat=transf_key(i+1:)
          else
             ori=transf_key(i+1:)
          end if
          mat=Get_Mat_From_Symb(cMat,cd(1:3))
          if (err_cfml%ierr /=0) then
             err_cfml%msg=" Bad matrix setting...: "//trim(err_cfml%msg)
          end if

          !> Origin
          if(present(v)) then
            v=0.0_cp
            Call Get_Separator_Pos(ori,",",pos,nc)
            if (nc /= 2) then
               err_cfml%ierr=1
               err_cfml%msg=" Bad origin setting...: "//trim(ori)
               return
            else
               v(1)=Read_Fract(ori(1:pos(1)-1))
               v(2)=Read_Fract(ori(pos(1)+1:pos(2)-1))
               v(3)=Read_Fract(ori(pos(2)+1:))
               if (err_cfml%ierr /=0) then
                  err_cfml%msg=" Bad origing setting...: "//trim(err_cfml%msg)//" :: "//trim(ori)
                  return
               end if
            end if
          end if
       else
          err_cfml%ierr=1
          err_cfml%msg=" No appropriate separator ("//cd(4)//") is present in the input string:"//trim(str)
       end if

    End Subroutine Get_Transf

    !!----
    !!---- GET_NUM
    !!----    Converts a string to numbers and write on VET/IVET if real/integer.
    !!----
    !!---- 05/04/2019
    !!
    Module Subroutine Get_Num(Str,vet,ivet,iv)
       !---- Argument ----!
       character (len=*),          intent ( in) :: Str   ! Input String to convert
       real(kind=cp), dimension(:),intent (out) :: vet   ! Vector of real numbers
       integer, dimension(:),      intent (out) :: ivet  ! Vector of integer numbers
       integer,                    intent (out) :: iv    ! Number of numbers in Vet/Ivet

       !---- Local variables ----!
       logical                   :: numero
       character (len=len(Str))  :: resto,cifre
       integer                   :: i,isum,ncharl,nchard,isegno,iniz,ipoi,idec,idig
       integer                   :: nchart, npos,nchard1,isum_exp,ioper
       real(kind=cp)             :: suma,segno,dec
       real(kind=cp)             :: sum_m

       !> Init
       iv=0
       ivet=0; vet=0.0
       call Clear_Error()
       resto=u_case(Str)
       do
          ioper=0
          isum_exp=0
          nchard1=0
          sum_m=0.0
          suma=0.0
          isum=0
          call cut_string(resto,ncharl,cifre,nchard)
          if (nchard <= 0) exit

          !> Is a number ?
          numero=.true.
          do i=1,nchard
             if (cifre(i:i) =='E') cycle
             npos=index(DIGIT,cifre(i:i))
             if (npos /= 0) cycle
             numero=.false.
          end do
          if (.not. numero) then
             iv=0
             !err_cfml%ierr=1
             !err_cfml%msg="The variable cannot be computed as a number in GET_NUM "
             return
          end if

          !---- Positive or Negative number ----!
          segno=1.0
          isegno=1
          iniz=1
          if (cifre(1:1) == DIGIT(12:12)) then
             segno=-1.0
             isegno=-1
             iniz=2
          end if

          !---- Decimal Number ----!
          ipoi=index(cifre(1:nchard),DIGIT(11:11))

          !---- Exponential Number ----!
          nchard1=index(cifre(1:nchard),"E")
          if (nchard1 /= 0) then
             nchart=nchard
             nchard=nchard1-1
          end if

          if (ipoi == 0) ipoi=nchard+1
          dec=real(ipoi-1-iniz)
          idec=ipoi-1-iniz
          do i=iniz,nchard
             idig=index(DIGIT,cifre(i:i))
             if (idig >= 1 .and. idig <= 11)  then
                if (idig <= 10)  then
                   suma=suma+real(idig-1)*10.0**dec
                   if (idec >= 0) isum=isum*10+(idig-1)
                   dec=dec-1.0
                   idec=idec-1
                end if
             else
                err_cfml%ierr=1
                err_cfml%msg="Limits of digit variable exceeded in GET_NUM"
                return
             end if
          end do

          if (nchard1 /= 0) then
             nchard1=nchard1+1
             select case (cifre(nchard1:nchard1))
                case ("-")
                   ioper=1
                   nchard1=nchard1+1

                case ("+")
                   nchard1=nchard1+1
             end select

             do i=nchard1,nchart
                idig=index(DIGIT,cifre(i:i))
                if (idig >= 1 .and. idig <= 10)  then
                   isum_exp=isum_exp*10+(idig-1)
                else
                   err_cfml%ierr=1
                   err_cfml%msg="Limits of digit variable exceeded in GET_NUM"
                   return
                end if
             end do
          end if

          iv=iv+1
          vet(iv)=suma*segno
          ivet(iv)=isum*isegno

          if (nchard1 /= 0) then
             select case (ioper)
                case (0)
                   sum_m=10.0**isum_exp

                case (1)
                   sum_m=10.0**isum_exp
                   sum_m=1.0/sum_m
             end select
             vet(iv)=vet(iv)*sum_m
          end if

          if (ncharl <= 0) then
             exit
          end if
       end do

    End Subroutine Get_Num

    !!----
    !!---- GET_NUMSTD
    !!----    Converts a string to a numbers with standard deviation with format:
    !!----        x.fffff(s)
    !!----
    !!---- 05/04/2019
    !!
    Module Subroutine Get_NumStd(Str, value, std, ic)
       !----Arguments ----!
       character(len=*),             intent( in) :: Str     ! Input String
       real(kind=cp), dimension(:),  intent(out) :: value   ! Vector of values with real numbers
       real(kind=cp), dimension(:),  intent(out) :: std     ! Vector of standard deviation values
       integer,                      intent(out) :: ic      ! Number of components of vector Value

       !---- Local Variables ----!
       character(len=len(Str))                :: resto,dire,numm
       integer                                :: iv,nlong,i
       integer                                :: np, np1, np2
       integer, dimension(size(value))        :: ivet
       real(kind=cp), dimension(size(value))  :: vet

       !> Init
       value=0.0_cp; std=0.0_cp
       ic=0
       call Clear_Error()
       !> Initial Checks
       if (len_trim(Str) == 0) then
          err_cfml%ierr=1
          err_cfml%msg="Blank line into Get_NumStd procedure"
          return
       end if

       i=index(Str,"!")
       if (i /= 0) then
          resto=adjustl(Str(1:i-1))
       else
          i=index(Str,"#")
          if (i /= 0) then
             resto=adjustl(Str(1:i-1))
          else
             resto=adjustl(Str)
          end if
       end if

       do
          if (len_trim(resto) == 0) exit
          call cut_string(resto,nlong,dire)
          np1=index(dire,"(")
          np2=index(dire,")")

          if ( (np2 < np1) .or.               &  ! ")" before than "("
               (np1 == 0 .and. np2 > 0) .or.     &  ! "(" doesn"t exists
               (np2 == 0 .and. np1 > 0) ) then      ! ")" doesn"t exists
             err_cfml%ierr=1
             err_cfml%msg="Wrong format using Standard values"
             return
          end if

          if (np1 == 0 .and. np2 == 0) then
             call get_num(dire,vet,ivet,iv)
             if (iv /= 1 .or. Err_CFML%ierr /=0) then
                err_cfml%ierr=1
                err_cfml%msg="Bad format in Get_NumStd procedure, directive -> "//trim(dire)//"  Input string:"//trim(Str)
                return
             end if
             ic=ic+1
             value(ic)=vet(1)
          else
             numm=dire(1:np1-1)
             np=index(numm,".")
             if (np == 0) then
                call get_num(numm,vet,ivet,iv)
                if (iv /= 1 .or. Err_CFML%ierr /=0) then
                   err_cfml%Ierr=1
                   err_cfml%msg="Bad format in Get_NumStd procedure, numm1 -> "//trim(numm)//"  Input string:"//trim(Str)
                   return
                end if
                ic=ic+1
                value(ic)=vet(1)
                numm=dire(np1+1:np2-1)
                call get_num(numm,vet,ivet,iv)
                if (iv /= 1) then
                   err_cfml%ierr=1
                   err_cfml%msg="Bad format in Get_NumStd procedure, numm2 -> "//trim(numm)//"  Input string:"//trim(Str)
                   return
                end if
                std(ic)=vet(1)
             else
                np=np1-np-1
                call get_num(numm,vet,ivet,iv)
                if (iv /= 1 .or. Err_CFML%ierr /=0) then
                   err_cfml%ierr=1
                   err_cfml%msg="Bad format in Get_NumStd procedure, numm3 -> "//trim(numm)//"  Input string:"//trim(Str)
                   return
                end if
                ic=ic+1
                value(ic)=vet(1)
                numm=dire(np1+1:np2-1)
                call get_num(numm,vet,ivet,iv)
                if (iv /= 1 .or. Err_CFML%ierr /= 0) then
                   err_cfml%ierr=1
                   err_cfml%msg="Bad format in Get_NumStd procedure, numm4 -> "//trim(numm)//"  Input string:"//trim(Str)
                   return
                end if
                std(ic)=vet(1)/(10.0_cp**np)
             end if
          end if
       end do

    End Subroutine Get_NumStd

    !!----
    !!---- NUMCOL_FROM_NUMFMT
    !!----    Provides the number of columns spanned by a numeric format field F,I,G,E
    !!----
    !!---- 05/04/2019
    !!
    Pure Module Function NumCol_from_NumFmt(Str) Result(n_col)
       !---- Argument ----!
       character (len=*), intent(in)  :: Str    ! Input format string
       integer                        :: n_col  ! Integer number of columns

       !---- Local variables ----!
       integer                         :: i,j,L,ncom,n1,n2,point,ier
       integer, dimension(0:len(Str))  :: pos
       character(len=len(Str))        :: fm
       character(len=10)              :: string

       !> Init
       n_col=0

       fm=U_case(adjustl(Str))
       fm=pack_string(fm)
       L=len_trim(fm)
       fm=fm(2:L-1)
       L=L-2
       ncom=0
       pos(0)=0
       do i=1,L
          if (fm(i:i) == ",") then
             ncom=ncom+1
             pos(ncom)=i
          end if
       end do
       ncom=ncom+1
       pos(ncom)=L+1
       n_col=0
       do i=1,ncom
          string=" "
          string=fm(pos(i-1)+1:pos(i)-1)
          point=index(string,".")
          if ( point /= 0) string=string(1:point-1)
          L=len_trim(string)
          do j=1,L
             point=index("FIGEX",string(j:j))
             if (point /= 0) then
                point=j
                exit
             end if
          end do
          n1=0
          Select Case (point)
             Case(0)
                n_col=0
                exit

             Case(1)
                string(point:point) = " "
                read(unit=string,fmt=*,iostat=ier) n2
                if (ier /= 0) n2=0
                n1=1

            Case default
               if (string(point:point)=="X") then
                  string(point:point) = " "
                  n1=1
                  read(unit=string,fmt=*,iostat=ier) n2
                  if (ier /= 0) n2=0
               else
                  string(point:point) = " "
                  read(unit=string,fmt=*,iostat=ier) n1,n2
                  if (ier /= 0) n2=0
               end if
          End Select
          n_col=n_col+n1*n2
       end do

    End Function NumCol_from_NumFmt

    !!----
    !!----  READ_FRACT
    !!----  Auxiliary subroutine for reading a string containing a real number
    !!----  or a fraction. Is able to handle simple symbols:"", "-", "+", means
    !!----  respectively: 1,-1,1
    !!----
    !!---- 05/04/2019
    !!
    Module Function Read_Fract(str) Result(value)
       !---- Arguments ----!
       Character(len=*), intent(in) :: str     ! Input String
       real(kind=cp)                :: value   ! Value

       !--- Local variables ---!
       integer       :: k, ierr
       real(kind=cp) :: num, den

       !> Init
       Value=0.0_cp
       if (len_trim(str) <= 0) return

       if (len_trim(str) == 1) then
          if (str == "+") then
             value=1.0_cp
             return

          else if(str == "-") then
             value=-1.0_cp
             return
          end if
       end if

       k=index(str,"/")
       if (k == 0) then !a single number
          read(unit=str,fmt=*,iostat=ierr) value
          if (ierr /= 0) then
             value=0.0_cp
             err_cfml%ierr=1
             err_cfml%msg=" The provided symbol is illegal: "//trim(str)
             return
          end if

       else !fraction
          read(unit=str(1:k-1),fmt=*,iostat=ierr) num
          if (ierr /= 0) then
             value=0.0_cp
             err_cfml%ierr=1
             err_cfml%msg=" The provided symbol is illegal: "//str(1:k-1)
             return
          end if

          read(unit=str(k+1:),fmt=*,iostat=ierr) den
          if (ierr /= 0) then
             value=0.0_cp
             err_cfml%ierr=1
             err_cfml%msg=" The provided symbol is illegal: "//str(k+1:)
             return
          end if

          value=num/den
       end if

    End Function Read_Fract

    !!----
    !!---- STRING_NUMSTD
    !!----    String with real value and standard deviation
    !!----    quoted in parenthesis
    !!----
    !!---- 05/04/2019
    !!
    Pure Module Function String_NumStd(Value, Std) Result(Str)
       !---- Argument ----!
       real(kind=cp),   intent(in)  :: Value    ! Value
       real(kind=cp),   intent(in)  :: Std      ! Standard deviation
       character(len=:),allocatable :: Str      ! String containing the information

       !---- Local Variables ----!
       character(len=10) :: fmtcar
       character(len=40) :: aux,sss
       integer           :: n,np,iy,long
       real(kind=cp)     :: y

       !> Init
       if (abs(std) < tiny(1.0_cp)) then
          if (abs(value) > 999999.0) then
             write(unit=aux,fmt=*) value
          else
             write(unit=aux,fmt="(f16.5)") value
          end if
          aux=adjustl(aux)
          if (aux(1:1) /= "-") aux=" "//trim(aux)
          str=trim(aux)

          return
       end if

       if (abs(value/std) > 1000000. .or. abs(value) < tiny(0.0_cp) )then
          if (abs(value) > 999999.0) then
             write(unit=aux,fmt=*) value
          else
             write(unit=aux,fmt="(f16.5)") value
          end if
          str=adjustl(aux)
          if (str(1:1) /= "-") str=" "//trim(str)
          return
       end if

       np=0
       y=std
       y=abs(std)       !RJA change for safety
       do
          if (y >= 2.0) exit
          np=np+1
          y=y*10.0
       end do

       ! iy=nint(y)   This is wrong, as it rounds down the esd
       if (y-int(y) > 0.09999)then
          iy=int(y)+1     !round up if next digit is non-zero
       else
          iy=int(y)       !keep first digit if next digit is zero
       end if

       if (iy == 20 .and. std < 1.0) then !Correction only for std numbers less than 1.0, eg. 0.020
          iy=2
          np=np-1
       end if

       aux=" "
       write(unit=aux,fmt=*) value
       str=trim(adjustl(aux))
       n=len_trim(str)
       if (n-np < 6) n=np+6
       fmtcar="f"
       if (n < 10) then
          write(unit=fmtcar(2:2),fmt="(i1)") n
       else
          write(unit=fmtcar(2:3),fmt="(i2)") n
       end if

       fmtcar=trim(fmtcar)//"."
       n=len_trim(fmtcar)
       if (np < 10) then
          write(unit=fmtcar(n+1:),fmt="(i1)") np
       else
          write(unit=fmtcar(n+1:),fmt="(i2)") np
       end if
       fmtcar="("//trim(fmtcar)//")"

       aux=" "
       write(unit=aux,fmt=fmtcar) value
       sss=trim(adjustl(aux))
       n=len_trim(sss)
       if (sss(n:n) == ".") sss(n:n)=" "

       sss=trim(sss)//"("
       n=len_trim(sss)
       np=len(sss)-n-1             !number of available places for writing
       aux=" "
       write(unit=aux,fmt=*) iy
       aux=pack_string(aux)
       long=len_trim(aux)
       if (long > np) then
          sss=sss(1:n-2)//"("//aux(1:np)//")"
       else
          sss=sss(1:n)//trim(aux)//")"
       end if
       aux=pack_string(sss)
       if (aux(1:1) /= "-") aux=" "//trim(aux)
       str=trim(aux)

    End Function String_NumStd

 End Submodule StringNum