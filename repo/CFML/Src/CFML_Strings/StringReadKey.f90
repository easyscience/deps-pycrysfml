!!----
!!---- SUBMODULE CFML_String_Utilities
!!----
!!----
!!
 Submodule (CFML_Strings) StringReadKey
   !---- Parameters ----!
   implicit none

 Contains
    !!----
    !!---- READ_KEY_STR
    !!----    Read a string on "filevar" starting with a particular "keyword"
    !!----    between lines "nline_ini" and "nline_end"
    !!----
    !!---- 05/04/2019
    !!
    Module Subroutine Read_Key_Str(filevar,nline_ini,nline_end,keyword,string,comment)
       !---- Arguments ----!
       character(len=*), dimension(:), intent(in)      :: filevar     ! Input vector of String
       integer,                        intent(in out)  :: nline_ini   ! Pointer to initial position to search
                                                                      ! Out -> Pointer to final position in search
       integer,                        intent(in)      :: nline_end   ! Pointer to final position to search
       character(len=*),               intent(in)      :: keyword     ! Word to search
       character(len=*),               intent(out)     :: string      ! Rest of the input string
       character(len=1), optional,     intent(in)      :: comment     ! Character that define a comment line

       !---- Local Variable ----!
       character(len=:), allocatable  :: line,linec
       character(len=:), allocatable  :: keyw
       character(len=1)               :: cc
       integer                        :: i,np,nt

       !> Init
       string=" "
       cc=' '
       if (present(comment)) cc=comment

       nt=min(size(filevar),nline_end)
       keyw=adjustl(keyword)
       keyw=l_case(keyw)

       do i=nline_ini,nt
          line=adjustl(filevar(i))
          if (len_trim(line) == 0 .or. line(1:1) == "!" .or. line(1:1) ==cc) cycle

          !> copy
          linec=line

          line=l_case(line)
          np=index(line,keyw)
          if (np == 0) cycle

          linec=linec(np:)
          call cut_string(linec)
          string=linec
          nline_ini=i
          exit
       end do

    End Subroutine Read_Key_Str

    !!----
    !!---- READ_KEY_STRVAL
    !!----    Read a string on "filevar" starting with a particular "keyword" between
    !!----    lines "nline_ini" and "nline_end".
    !!----    If the string contains numbers they are read and put into "vet/ivet".
    !!----    The variable "string" contains the input string without the "keyword"
    !!----    "string" contains the input string without the "keyword".
    !!----
    !!---- 05/04/2019
    !!
    Module Subroutine Read_Key_StrVal(filevar,nline_ini,nline_end,keyword,string,vet,ivet,iv,comment)
       !---- Arguments ----!
       character(len=*), dimension(:),           intent(in)      :: filevar       !  In -> Input vector of String
       integer,                                  intent(in out)  :: nline_ini     !  In -> Pointer to initial position to search
                                                                                  ! Out -> Pointer to final position in search
       integer,                                  intent(in)      :: nline_end     !  In -> Pointer to final position to search
       character(len=*),                         intent(in)      :: keyword       !  In -> Word to search
       character(len=*),                         intent(out)     :: string        ! Out -> Rest of the input string
       real(kind=cp),dimension(:),     optional, intent(out)     :: vet           ! Out -> Vector for real numbers
       integer,dimension(:),           optional, intent(out)     :: ivet          ! Out -> Vector for integer numbers
       integer,                        optional, intent(out)     :: iv            ! Out -> Number of numbers
       character(len=1),               optional, intent(in)      :: comment       ! Character that define a comment line

       !---- Local Variable ----!
       logical                        :: sval
       character(len=:), allocatable  :: line,linec
       character(len=:), allocatable  :: keyw
       character(len=1)               :: cc
       integer                        :: i,np,nt

       !> Init
       cc=' '
       if (present(comment)) cc=comment

       nt=min(size(filevar),nline_end)
       string=" "
       keyw=adjustl(keyword)
       keyw=l_case(keyw)
       sval=.false.

       if (present(vet) .and. present(ivet) .and. present(iv)) sval=.true.
       if (sval) then
          vet=0.0_cp
         ivet=0
           iv=0
       end if

       do i=nline_ini,nt
          line=adjustl(filevar(i))
          if (len_trim(line) == 0 .or. line(1:1)=="!" .or. line(1:1)==cc) cycle
          linec=line

          line=l_case(line)
          np=index(line,keyw)
          if (np == 0) cycle

          linec=linec(np:)
          call cut_string(linec)
          string=linec
          nline_ini=i
          exit
       end do

       if (sval .and. (len_trim(string) > 0) ) then
          line=string

          !---- Values ----!
          call get_num(line,vet,ivet,iv)
          if (iv <=0) then
              vet=0.0_cp
             ivet=0
          end if
       end if

    End Subroutine Read_Key_StrVal

    !!----
    !!---- READ_KEY_VALUE
    !!----    Read a string on "filevar" starting with a particular "keyword" between
    !!----    lines "nline_ini" and "nline_end".
    !!----    If the string contains numbers they are read and put into "vet/ivet".
    !!----
    !!---- 05/04/2019
    !!
    Module Subroutine Read_Key_Value(filevar,nline_ini,nline_end,keyword,vet,ivet,iv,comment,line_key)
       !---- Arguments ----!
       character(len=*), dimension(:), intent(in)     :: filevar            !  In -> Input vector of String
       integer,                        intent(in out) :: nline_ini          !  In -> Pointer to initial position to search
                                                                            ! Out -> Pointer to final position in search
       integer,                        intent(in)     :: nline_end          !  In -> Pointer to final position to search
       character(len=*),               intent(in)     :: keyword            !  In -> Word to search
       real(kind=cp),dimension(:),     intent(out)    :: vet                ! Out -> Vector for real numbers
       integer,dimension(:),           intent(out)    :: ivet               ! Out -> Vector for integer numbers
       integer,                        intent(out)    :: iv                 ! Out -> Number of components
       character(len=1),     optional, intent(in)     :: comment            ! Consider the character passed in comment as a comment to skip the line
       character(len=*),     optional, intent(out)    :: line_key           ! Out -> Cut line where keyword is read

       !---- Local Variable ----!
       character(len=:), allocatable  :: line
       character(len=:), allocatable  :: keyw
       character(len=1)               :: cc
       integer                        :: i,np,nt

       !---- Initial value ----!
       cc=' '
       if (present(comment)) cc=comment

       nt=min(size(filevar),nline_end)
       iv  = 0
       vet = 0.0_cp
       ivet= 0
       keyw=adjustl(keyword)
       keyw=l_case(keyw)

       do i=nline_ini,nt
          np=0
          line=adjustl(filevar(i))
          if (len_trim(line) == 0 .or. line(1:1) == "!" .or. line(1:1)==cc) cycle

          line=l_case(line)
          np=index(line,keyw)
          if (np == 0) cycle

          line=line(np:)
          call cut_string(line)
          call get_num(line,vet,ivet,iv)
          if (present(line_key)) line_key=line
          if (Err_CFML%ierr /=0) exit
          nline_ini=i
          exit
       end do

    End Subroutine Read_Key_Value

    !!----
    !!---- READ_KEY_VALUEST
    !!----    Read parameters and standard deviation on the line of "filevar" starting
    !!----    with a particular "keyword".
    !!----    The search is done between lines "nline_ini" and "nline_end".
    !!----
    !!---- 05/04/2019
    !!
    Module Subroutine Read_Key_ValueSTD(filevar,nline_ini,nline_end,keyword,vet1,vet2,iv,comment)
       !---- Arguments ----!
       character(len=*), dimension(:),  intent(in)     :: filevar         !  In -> Input vector of String
       integer,                         intent(in out) :: nline_ini       !  In -> Pointer to initial position to search
                                                                          ! Out -> Pointer to final position in search
       integer,                         intent(in)     :: nline_end       !  In -> Pointer to final position to search
       character(len=*),                intent(in)     :: keyword         !  In -> Word to search
       real(kind=cp),dimension(:),      intent(out)    :: vet1            ! Out -> Vector of real numbers
       real(kind=cp),dimension(:),      intent(out)    :: vet2            ! Out -> Vector of standard deviations
       integer,                         intent(out)    :: iv              ! Out -> Number of components
       character(len=1),      optional, intent(in)     :: comment         ! Consider the character passed in comment as a comment to skip the line

       !---- Local Variable ----!
       character(len=:), allocatable  :: line
       character(len=:), allocatable  :: keyw
       character(len=1)               :: cc
       integer                        :: i,np,nt

       !---- Initial value ----!
       cc=' '
       if (present(comment)) cc=comment
       nt=min(size(filevar),nline_end)
       iv  = 0
       vet1 = 0.0_cp
       vet2 = 0.0_cp
       keyw=adjustl(keyword)
       keyw=l_case(keyw)

       do i=nline_ini,nt
          line=adjustl(filevar(i))
          if (len_trim(line) == 0 .or. line(1:1) == "!" .or. line(1:1)==cc) cycle
          line=l_case(line)
          np=index(line,keyw)
          if (np == 0) cycle
          line=line(np:)
          call cut_string(line)  !Eliminates the keyword
          line=Eliminate_Non_Numeric_except_par(line)
          call get_numstd(line,vet1,vet2,iv)
          if (Err_CFML%ierr /= 0) exit
          nline_ini=i
          exit
       end do

    End Subroutine Read_Key_ValueSTD

    Function Eliminate_Non_Numeric_except_par(string) result(newstring)
       character(len=*), intent(in) :: string
       character(len=:), allocatable :: newstring
       integer :: i
       newstring=trim(string)
       do i=1,len_trim(newstring)
          if(index(DIGIT_EXT,newstring(i:i)) == 0) newstring(i:i)=" "
       end do
    end Function Eliminate_Non_Numeric_except_par

 End Submodule StringReadKey