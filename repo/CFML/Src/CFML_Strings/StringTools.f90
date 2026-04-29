!!----
!!---- SUBMODULE CFML_String_Utilities
!!----
!!----
!!
Submodule (CFML_Strings) StringTools
   !---- Parameters ----!
   implicit none

 Contains
   !!----
   !!---- PACK_STRING
   !!----    Pack a string: the function provides a string without empty spaces
   !!----
   !!---- 05/04/2019
   !!
   Pure Module Function Pack_String(Str) Result (Strp)
      !---- Argument ----!
      character(len=*), intent(in) :: str    ! Input String
      character(len=len_trim(str)) :: strp   ! Output string

      !---- Local variables ----!
      integer ::  i,n

      n=0
      strp=" "
      do i=1,len(str)
         if (str(i:i) /= " ") then
            n=n+1
            strp(n:n)=str(i:i)
         end if
      end do

   End Function Pack_String

   !!----
   !!---- STRING_COUNT
   !!----    counting the number of times a substring appears in a string
   !!----
   !!---- 05/04/2019
   !!
   Pure Module Function String_Count(str,substr) Result(N)
      !---- Arguments ----!
      character(len=*), intent(in) :: str       ! Input String
      character(len=*), intent(in) :: substr    ! Substring model
      integer                      :: N         ! Number

      !---- Local variables ---!
      character(len=len_trim(str)) :: string
      integer                      :: i,lstr

      !> Init
      N=0

      !lstr=len_trim(substr)-1
      lstr=len_trim(substr)        ! Changed by JGP 07/05/2019
      string=str
      do
         i=index(string,trim(substr))
         if (i == 0) exit

         n=n+1
         string=string(i+lstr:)
      end do

   End Function String_Count

   !!----
   !!---- L_CASE
   !!----    Conversion to lower case, text is not modified
   !!----
   !!---- 05/04/2019
   !!
   Pure Module Function L_Case(Str) Result (LStr)
      !---- Argument ----!
      character (len=*), intent(in)  :: Str    ! Input String
      character (len=:), allocatable :: LStr   ! lower case of Text

      !---- Local variables ----!
      integer, parameter :: INC = ICHAR("A") - ICHAR("a")
      integer            :: leng, pos

      lstr=str
      leng=len_trim(lstr)
      do pos=1,leng
         if (lstr(pos:pos) >= "A" .and. lstr(pos:pos) <= "Z")           &
             lstr(pos:pos) = CHAR ( ICHAR(lstr(pos:pos)) - INC )
      end do

   End Function L_Case

   Pure Module Function Num_Items(string,separator) result(nitems)
      character(len=*), intent (in)           :: string
      character(len=1), intent (in), optional :: separator
      integer   :: nitems
      !---- Local variables ----!
      integer :: ls,i
      character(len=1) :: sep
      character(len=len(string)) :: line

      nitems = 0
      ls=min(len_trim(string),len(string)-1)
      if(ls == 0) return
      sep=" "
      line=adjustl(string)
      if(present(separator)) sep=separator
      nitems = 1
      do i=1,ls
        if(line(i:i) == sep .and. line(i+1:i+1) /= sep) nitems = nitems + 1
      end do

   End Function Num_Items

   !!----
   !!---- U_CASE
   !!----    Conversion to upper case, text is not modified
   !!----
   !!---- 05/04/2019
   !!
   Pure Module Function U_Case(Str) Result (UStr)
      !---- Argument ----!
      character (len=*), intent(in)  :: Str   ! Input string
      character (len=:), allocatable :: UStr  ! Upper conversion

      !---- Local variables ----!
      integer, parameter :: INC = ICHAR("A") - ICHAR("a")
      integer            :: leng, pos

      UStr=Str
      leng=len_trim(UStr)
      do pos=1,leng
         if (UStr(pos:pos) >= "a" .and. UStr(pos:pos) <= "z")           &
             UStr(pos:pos) = CHAR ( ICHAR(UStr(pos:pos)) + INC )
      end do

   End Function U_Case

   !!----
   !!---- STRIP_STRING
   !!----     Strip a string from a particular word
   !!----
   !!---- 05/04/2019
   !!
   Pure Module Function Strip_String(str, to_strip) Result(sstr)
      !---- Arguments----!
      character(len=*), intent(in) :: str            ! Input string
      character(len=*), intent(in) :: to_strip       ! Pattern
      character(len=len_trim(str)) :: sstr

      !---- Local variables ----!
      integer :: i

      sstr=trim(str)

      i=index(str,trim(to_strip),back=.true.)
      if (i > 0) sstr=str(1:i-1)

   End Function Strip_String

   !!----
   !!---- EQUAL_SETS_TEXT
   !!----    Determine if two sets of text lines are equal irrespective of the
   !!----    order of the lines.
   !!----
   !!----    The function is true if the two sets of text
   !!----    have the same lines in whatever order.  Two lines are equal only
   !!----    if they have the same length and all their component characters
   !!----    are equal and in the same order.
   !!----
   !!---- 05/04/2019
   !!
   Pure Module Function Equal_Sets_Text(str1,n1,str2,n2) result(Equal)
      !---- Arguments ----!
      character(len=*), dimension(:), intent(in) :: str1              ! Vector of String
      character(len=*), dimension(:), intent(in) :: str2              ! Vector of String
      integer,                        intent(in) :: n1                ! Number of lines on Text1
      integer,                        intent(in) :: n2                ! Number of lines on str2
      logical                                    :: Equal  !

      !---- Local variables ----!
      integer :: i,j
      logical :: info

      Equal=.false.

      if (n1 /= n2) return
      if (len(str1) /= len(str2)) return

      do i=1,n1
         info=.false.
         do j=1,n2
            if (str1(i) == str2(j)) then
               info=.true.
               exit
            end if
         end do
         if (.not. info) return
      end do

      Equal=.true.

   End Function Equal_Sets_Text

   !!----
   !!---- GET_DATETIME
   !!----    Define a string containing the information about Date and Time
   !!----
   !!---- 05/04/2019
   !!
   Module Function Get_DateTime() Result(Str)
      !---- Argument ----!
      character(len=:), allocatable :: Str  ! String containing the Date and Time

      !---- Local Variables ----!
      character(len=10) :: dat
      character(len=10) :: tim

      call date_and_time(date=dat,time=tim)

      Str="Date: "//dat(7:8)//"/"//dat(5:6)//"/"//dat(1:4)//      &
          "  Time: "//tim(1:2)//":"//tim(3:4)//":"//tim(5:10)

   End Function Get_DateTime

   !!----
   !!---- GET_DIRNAME
   !!----     Obtain the Path from a string
   !!----
   !!---- 05/04/2019
   !!
   Pure Module Function Get_Dirname(Str) Result(Directory)
      !---- Argument ----!
      Character(Len=*), Intent (In)  :: Str          ! String containing Path + Filename
      Character(Len=:), allocatable  :: Directory    ! Path

      !---- Local Variables ----!
      Integer :: i

      i=index(str, OPS_SEP, Back=.True.)
      if (i > 0) then
         Directory=str(1:I)
      else
         Directory=" "
      end If

   End Function Get_Dirname

   !!----
   !!---- GET_FILENAME
   !!----    Get Filename from a String containing in general Path+Filename
   !!----
   !!---- 05/04/2019
   !!
   Pure Module Function Get_Filename(Str) Result(Filename)
      !---- Argument ----!
      character(Len=*), intent(in)  :: Str       ! String containing Path + Filename
      character(Len=:), allocatable :: Filename  ! Filename

      !---- Local Variables ----!
      integer :: i

      i=index(str, OPS_SEP, Back = .True.)
      if (i > 0) then
         Filename=Str(i+1:)
      else
         Filename=trim(Str)
      end If

   End Function Get_Filename

   !!----
   !!---- GET_EXTENSION
   !!----    Obtaining the extension of the Filename
   !!----
   !!---- 05/04/2019
   !!
   Pure Module Function Get_Extension(filename, dotted) Result(extension)
      !---- Arguments ----!
      character(len=*),  intent(in)  :: filename     ! Input filename
      logical, optional, intent(in)  :: dotted       ! If True, the extension will be returned with a dot
      character(len=:), allocatable  :: extension    ! Extension of the file

      !---- Local Variables ----!
      integer :: idx
      logical :: dot

      !> Search for the last dot.
      idx=index(filename, '.', back=.true.)

      !> If no dot was found in the filename, then the file has no extension.
      if (idx == 0) then
          extension=" "
      else
          !> Handle the optional dotted argument.
          dot=.false.
          if (present(dotted)) dot=dotted

          if (.not. dot) idx = idx + 1

          ! The extension is set.
          extension=filename(idx:)
      end if

   End Function Get_Extension

   !!----
   !!---- GET_SEPARATOR_POS
   !!----    Determines the positions of the separator character "car" in string "Line" and generates
   !!----    the vector Pos containing the positions. The number of times the character "car" appears
   !!----    In "Line" is stored in "ncar".
   !!----    The separator "car" is not counted within substrings of "Line" that are written within
   !!----    quotes. The following example illustrates the functionning of the subroutine
   !!----
   !!----       !       12345678901234567890123456789012345678901234567890
   !!----        line =' 23, "List, of, authors", this book, year=1989'
   !!----
   !!----    A call like:  call Get_Separator_Pos(line,',',pos,ncar) provides
   !!----    ncar= 3
   !!----    pos= [ 4, 25, 36, 0, ....]
   !!----
   !!---- 05/04/2019
   !!
   Pure Module Subroutine Get_Separator_Pos(Str,car,pos,ncar)
      !---- Arguments ----!
      character(len=*),      intent(in)  :: Str   ! Inout String
      character(len=1),      intent(in)  :: car   ! Separator character
      integer, dimension(:), intent(out) :: pos   ! Vector with positions of "sep" in "Line"
      integer,               intent(out) :: ncar  ! Number of appearances of "sep" in "Line"

      !---- Local variables ----!
      integer :: i,j,k

      !> init
      ncar=0
      pos=0

      j=0
      do i=1,len_trim(str)
         j=j+1
         if (str(j:j) == '"') then  !A chains of characters is found, advance up to the next "
            do k=1,len_trim(str)    !the character "car" is ignored if it is within " "
               j=j+1
               if(j > len_trim(str)) exit
               if (str(j:j) /= '"') cycle
               exit
            end do
         end if

         if (str(j:j) == car) then
            ncar=ncar+1
            pos(ncar)=j
         end if
      end do

   End Subroutine Get_Separator_Pos

   !!----
   !!---- GET_WORDS
   !!----    Determines the number of words (Ic) in the string "Line" and generates a
   !!----    character vector "Dire" with separated words.
   !!----    Control of errors is possible by inquiring the global variables Err_CFML
   !!----    and Err_CFML_Mess. The last modification allows to treat strings between
   !!----    quotes as a single word.
   !!----
   !!---- 05/04/2019
   !!
   Module Subroutine Get_Words(Str,dire,ic,sep)
      !---- Argument ----!
      character(len=*),                 intent ( in) :: Str   ! Input string
      character(len=*), dimension(:),   intent (out) :: dire  ! Vector of Words
      integer,                          intent (out) :: ic    ! Number of words
      character(len=*), optional,       intent ( in) :: sep   ! separator other than blank

      !---- Local variables ----!
      character (len=len(Str))  :: line1,line2
      integer                   :: nlong2
      integer                   :: ndim, j

      !> Init
      ic=0
      ndim=size(dire)
      line1=Str
      !Remove tabs, or a provided separator, replacing them by blanks
      do j=1,len_trim(line1)
        if(line1(j:j) == char(9)) line1(j:j)=" "
        if(present(sep)) then
           if(line1(j:j) == sep) line1(j:j)="  "
        end if
      end do

      do
         line1=adjustl(line1)
         if(line1(1:1) == '"') then
            j=index(line1(2:),'"')
            if( j > 0) then
              line2=line1(2:j)
              nlong2=len_trim(line2)
              line1 = line1(j+2:)
            else
              err_cfml%ierr=1
              err_cfml%msg="Non balanced quotes!"
              exit
            end if
         else
            call cut_string(line1,str2=line2,nlong2=nlong2)
         end if
         if (nlong2 == 0) exit
         ic=ic+1
         if (ic > ndim) then
            err_cfml%ierr=1
            err_cfml%msg="Dimension of DIRE exceeded"
            exit
         end if
         dire(ic)=adjustl(line2(:nlong2))
      end do

   End Subroutine Get_Words

   !!----
   !!---- CUTST
   !!----    Removes the first word of the input String.
   !!----    Provides (optionally) a string with the first word.
   !!----
   !!---- 05/04/2019
   !!
   Pure Module Subroutine Cut_String(Str1,nlong1,Str2,nlong2)
      !---- Argument ----!
      character(len=*),           intent(in out) :: Str1     ! Input string / Out: string without the first word
      character(len=*), optional, intent(   out) :: Str2     ! The first word of String on Input
      integer,          optional, intent(   out) :: nlong1   ! Give the length of Str1 on Output
      integer,          optional, intent(   out) :: nlong2   ! Give the length of Str2 on Output

      !---- Local variables ----!
      integer  :: k,iniz1

      !---- Initializing variables ----!
      if (present(nlong1)) nlong1=0
      if (present(nlong2)) nlong2=0

      !---- Initializing to blank the directive ----!
      if (present(str2)) str2=" "

      !---- Elimination of possible blanks on the left ----!
      str1=adjustl(str1)
      if (len_trim(str1) <= 0) return

      k=len(str1)
      iniz1=index(str1," ")

      if (k ==1) then
         if (present(str2)) str2=str1
         if (present(nlong2)) nlong2=1
         str1=" "
      else
         if (iniz1 > 0) then
            if (present(str2))  str2=str1(1:iniz1-1)
            if (present(nlong2)) nlong2=len_trim(str1(1:iniz1-1))
            str1=str1(iniz1:)
         else
            if (present(str2))  str2=str1
            if (present(nlong2)) nlong2=len_trim(str1)
            str1=" "
         end if
      end if

      str1=adjustl(str1)
      if(present(nlong1)) nlong1=len_trim(str1)

   End Subroutine Cut_String

   !!----
   !!---- GET_SUBSTRING_POSITIONS
   !!----    Determines the positions of the substring "substr" in "String" and generates
   !!----    the vector Pos containing the positions of the first character of "substr" in "String".
   !!----    The number of times the "substr" appears in "String" is stored in "nsubs".
   !!----
   !!---- 05/04/2019
   !!
   Pure Module Subroutine Get_Substring_Positions(str,substr,pos,nsubs)
      !---- Arguments ----!
      character(len=*),      intent(in)  :: str         ! In -> Input String
      character(len=*),      intent(in)  :: substr      ! In -> Substring
      integer, dimension(:), intent(out) :: pos         ! Out -> Vector with positions of the firs character of "substr" in "String"
      integer,               intent(out) :: nsubs       ! Out -> Number of appearance of "substr" in "String"

      !---- Local Variables ----!
      integer :: i,j,lsubs

      !> init
      nsubs=0
      pos=0

      lsubs=len_trim(substr)
      j=0
      do i=1,len_trim(str)
         j=j+1
         if (str(j:j+lsubs-1) == trim(substr)) then
            nsubs=nsubs+1
            pos(nsubs)=j
         end if
      end do

   End Subroutine Get_Substring_Positions

   !!----
   !!---- SUBSTRING_REPLACE
   !!----    Subroutine to replace a substring (substr) by another one (rep_string)
   !!----    within a given string (string). The original string is modified on output.
   !!----    If len_trim(warning) /= 0, one of the substrings will not be complete,
   !!----    it works as a warning or error condition without interrupting the
   !!----    procedure.
   !!----
   !!---- 05/04/2019
   !!
   Pure Module Subroutine SubString_Replace(string, substr, repstr, warning)
      !---- Arguments ----!
      character(len=*), intent(in out) :: string   ! Input/output string
      character(len=*), intent(in)     :: substr   ! Subtring to be replaced
      character(len=*), intent(in)     :: repstr   ! String for add
      character(len=*), intent(out)    :: warning  ! Message

      ! --- Local variables ---!
      integer                                      :: i,j,lstr,ncount,nsubs,d,dmax
      integer,            dimension(:),allocatable :: pos
      character(len=1024),dimension(:),allocatable :: splitted_string

      !> Init
      warning=" "

      lstr=len(substr)
      i=index(repstr,substr)
      if (i /= 0) then
         !> Check if the substring to be replaced is contained in the replacing string
         !> In such case the alternative short code doesn't work ... we have to use the longer analysis below
         ncount=String_Count(string,trim(substr))+1
         allocate(pos(ncount))
         allocate(splitted_string(ncount))
         call Get_Substring_Positions(string,substr,pos,nsubs)

         dmax=0
         do i=2,nsubs
            d=pos(i)-pos(i-1)
            if (d > dmax) dmax=d
         end do
         if (dmax > 1024) write(unit=warning,fmt="(a)") " => Warning! ... string too long to be fetch into the splitted_string"

         !> Construct the splitted string
         j=1
         splitted_string(j)=string(1:pos(j)-1)
         do
            j=j+1
            if (j > nsubs) exit
            splitted_string(j)=string(pos(j-1)+lstr:pos(j)-1)
         end do
         splitted_string(ncount)=string(pos(nsubs)+lstr:)

         !> Construct now the full string
         string=""
         do i=1,nsubs
            string=trim(string)//trim(splitted_string(i))//repstr
         end do
         string=trim(string)//trim(splitted_string(ncount))

      else
         !> The following short code works easily when substr is not contained in repstr
         do
            i=index(string,substr)
            if (i == 0) exit
            string=string(1:i-1)//repstr//trim(string(i+lstr:))
         end do
      end if

   End Subroutine SubString_Replace

   !!----
    !!---- SORT_STRINGS
    !!----    Sort an array of string
    !!----
    !!---- 03/04/2019
    !!
    Recursive Module Subroutine Sort_Strings(Str)
       !---- Argument ----!
       character(len=*), dimension(:), intent(in out) :: Str

       !---- Local variables ----!
       integer :: iq

       if (size(Str) > 1) then
          call Sort_PR_Partition(Str, iq)
          call Sort_Strings(Str(:iq-1))
          call Sort_Strings(Str(iq:))
       end if

    End Subroutine Sort_Strings

    !!----
    !!---- SORT_PR_PARTITION
    !!----
    !!----    (Private)
    !!----    Utilised by Sort_Strings.
    !!----
    !!---- 03/04/2019
    !!
    Pure Module Subroutine Sort_PR_Partition(A, Marker)
       !---- Arguments ----!
       character(len=*), dimension(:), intent(in out) :: A
       integer,                        intent(   out) :: marker

       !---- Local variables ----!
       integer                  :: i, j
       character(len=len(A(1))) :: temp
       character(len=len(A(1))) :: x      ! pivot point

       x = A(1)
       i= 0
       j= size(A) + 1

       do
          j = j-1
          do
             if (A(j) <= x) exit
             j = j-1
          end do
          i = i+1
          do
             if (A(i) >= x) exit
             i = i+1
          end do
          if (i < j) then
             !---- exchange A(i) and A(j)
             temp = A(i)
             A(i) = A(j)
             A(j) = temp
          else if (i == j) then
             marker = i+1
             return
          else
             marker = i
             return
          end if
       end do

    End Subroutine Sort_PR_Partition

End Submodule StringTools