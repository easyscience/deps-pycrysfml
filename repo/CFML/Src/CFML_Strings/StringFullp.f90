!!----
!!---- SUBMODULE CFML_String_Utilities
!!----
!!----
!!
Submodule (CFML_Strings) StringFullp
   !---- Parameters ----!
   implicit none

   !> PARAMETERS
   character(len=*),  parameter :: CTAB          = Char(9)         ! Character parameter for TAB
   Integer ,          parameter :: IENDFMT       = 0               ! Integer parameter for EndFMT
   Integer ,          parameter :: IINTE         =-1               ! Integer parameter for iInte
   Integer ,          parameter :: IREAL         =-2               ! Integer parameter for iReal
   Integer ,          parameter :: I_NINE        =57               ! Integer parameter for ASCII code for Nine
   Integer ,          parameter :: I_ONE         =49               ! Integer parameter for ASCII code for One
   Integer ,          parameter :: I_ZERO        =48               ! Integer parameter for ASCII code for Zero

   Integer ,          parameter :: IERREOF       =-1               ! Integer parameter for Error code
   Integer ,          parameter :: IERRNONE      = 0
   Integer ,          parameter :: IERRFIELDS    = 1
   Integer ,          parameter :: IERRIO        = 2
   Integer ,          parameter :: IERRFIELDTYPE = 3
   Integer ,          parameter :: IERRCHARBEGG  = 4
   Integer ,          parameter :: IERRINVALC    = 5
   Integer ,          parameter :: IERRINVALFIELD= 6
   Integer ,          parameter :: IERRINVALCHAR = 7
   Integer ,          parameter :: IERREMPTYFIELD= 8
   Integer ,          parameter :: IERRSTRLENGTH = 9
   Integer ,          parameter :: IERRSEPMISS   =10
   Integer ,          parameter :: IERREFRMT     =11
   Integer ,          parameter :: IERRNUMBER    =12

   !> Variables
   Integer :: Line_Nb       ! Line number (findFMT)

 Contains

    !!--++
    !!--++ BUILDFMT
    !!--++
    !!--++    (PRIVATE)
    !!--++    Add a new field to the FMT string
    !!--++
    !!--++ 05/04/2019
    !!
    Module Subroutine BuildFMT(iFld,nCar,nStr,FMTstring)
       !---- Arguments ----!
       Integer,           intent(in    ) ::   iFld       ! Format type
       Integer,           intent(in out) ::   nCar       ! integer/real field: number of characters in field
                                                         ! character field: number of characters to skip before A field
       Integer,           intent(in out) ::   nStr       ! current character number in FMTstring
       Character (len=*) ,intent(in out) ::   FMTstring  ! FORTRAN format string

       !---- Local variables ----!
       Integer ::  N

       !> heading symbol "F"
       nStr = nStr + 1
       if (nStr > Len(FMTstring)) then
          Err_CFML%Ierr = iErrStrLength          ! format string length exceeded
          return
       end if

       if (iFld == iInte) then
          FMTstring(nStr:nStr)  = "i"   !descriptor are in lower case to be F-compatible
       else if (iFld == iReal) then
          FMTstring(nStr:nStr)  = "f"
       else if (iFld > 0) then
          if (nCar == 0) then
             FMTstring(nStr:nStr)  = "a"
          else
             if (nCar < 10) then
                write(unit=FMTstring(nStr:),fmt="(a,i1,a)") "tr",nCar,",a"
             else
                write(unit=FMTstring(nStr:),fmt="(a,i2,a)") "tr",nCar,",a"
             end if
             nStr=len_trim(FMTstring)
          end if
       end if

       !> numeric part of Integer and real fields
       if (iFld < 0) then
          !> hundredth
          if (nCar >= 100) then
             N = Int(nCar/100)
             nStr = nStr + 1
             if (nStr > Len(FMTstring)) then
                Err_CFML%Ierr = iErrStrLength          ! format string length exceeded
                return
             end if
             FMTstring(nStr:nStr) = Char(N+48)
             nCar = nCar - N*100
          end if

          !> tenth
          if (nCar >= 10) then
             N = Int(nCar/10)
             nStr = nStr + 1
             if (nStr > Len(FMTstring)) then
                Err_CFML%Ierr = iErrStrLength          ! format string length exceeded
                return
             end if
             FMTstring(nStr:nStr) = Char(N+48)
             nCar = nCar - N*10
          end if

          !> units
          nStr = nStr + 1
          if (nStr > Len(FMTstring)) then
             Err_CFML%Ierr = iErrStrLength          ! format string length exceeded
             return
          end if
          FMTstring(nStr:nStr) = Char(nCar+48)

          !> Add ".0" to the end of real fields
          if (iFld == iReal) then
             nStr = nStr + 2
             if (nStr > Len(FMTstring)) then
                Err_CFML%Ierr = iErrStrLength          ! format string length exceeded
                return
             end if
             FMTstring(nStr-1:nStr) = ".0"
          end if

       else if (iFld > 0) then
          !> numeric part of "A" fields
          nStr = nStr + 1
          if (nStr > Len(FMTstring)) then
             Err_CFML%Ierr = iErrStrLength          ! format string length exceeded
             return
          end if
          if(iFld <= i_Nine) then
            FMTstring(nStr:nStr)   = Char(iFld)
          else
            write(unit=FMTstring(nStr:),fmt="(i2)") iFld-48
            nStr=len_trim(FMTstring)
          end if
       end if

       !> Add a separator "," after each new FORTRAN field
       nStr = nStr + 1
       if (nStr > Len(FMTstring)) then
          Err_CFML%Ierr = iErrStrLength          ! format string length exceeded
          return
       end if
       FMTstring(nStr:nStr) = ","

    End Subroutine BuildFMT

    !!----
    !!---- FINDFMT
    !!----
    !!----    The routine "FindFmt" emulates the free format data input
    !!----    Read(unit=String1,fmt="(a,i,2f,..)") aString,i1,R1,R2,...
    !!----    but with additional error checking. Thus, given a description
    !!----    of the expected fields "FindFmt" returns the format of the line
    !!----    to be decoded. Valid field descriptors are:
    !!----    I:integer; R:real; A:free A format; 1 to 14:A1 to A14
    !!----
    !!----    In the previous versions of this procedure the FMTfields contained
    !!----    digits for telling the program the maximum expected number of
    !!----    characters in a keyword. This limited the maximum length of the
    !!----    keyword to 9. In this version we have extended this up to 14, using
    !!----    the convention a=10, b=11, c=12, d=13 and e=14.
    !!----    Examples:
    !!----      FMTFields='dii9ff'
    !!----      -> expect to read String1(1:13), 2 integers, String2(1:9) and 2 reals
    !!----
    !!----    This routine have an associated FindFMT error code (iErr_fmt)
   !!----      -2 : FORTRAN read error
    !!----      -1 : End of file
    !!----       0 : No Error
    !!----       1 : empty format descriptor (0 field)
    !!----       2 : data string read error
    !!----       3 : integer field found real !
    !!----       4 : begged dot, sign or "e" character !
    !!----       5 : invalid character in an integer field !
    !!----       6 : invalid field in format descriptor !
    !!----       7 : invalid character in a numeric field !
    !!----       8 : 0 character in current field !
    !!----       9 : format string length exceeded !
    !!----      10 : separator missing !
    !!----      11 : incomplete E or D format !
    !!----      12 : incomplete number !
    !!----
    !!----   An error message is generated and written to the public variable "Mess_FindFMT"
    !!----   Consult the structure of Mess_FindFMT that is of type: Err_Text_Type.
    !!-->>
    !!--..   Example of use:
    !!--..       Character aLine*(*),FMTfields*(*),FMTstring*(*),String*5
    !!--..       Parameter (iLun=30)       ! input logical unit number
    !!--..
    !!--..    !-- Usual fixed format input (e.g.)
    !!--..    Read(unit=iLun,fmt="(4x,a5,i3,1x,2f8.2,i5)") String,i1,R1,R2,i2
    !!--..
    !!--..    !-- Free format input (Read performed by FindFMT)
    !!--..       FMTfields = "5iffi"
    !!--..       Call FindFmt(Lun,aLine,FMTfields,FMTstring)
    !!--..       if (iErr_fmt == -1) GoTo 998  ! End of Line| Block treating
    !!--..       if (iErr_fmt /= 0)  GoTo 999  ! input error|   errors
    !!--..       Read(unit=aLine,fmt=FMTstring) String,i1,R1,R2,i2
    !!--..
    !!--..    !-- Free format input (Read performed by calling routine)
    !!--..       Read(unit=iLun,fmt="(a)") aLine
    !!--..       FMTfields = "5iffi"
    !!--..       Call FindFmt(0,aLine,FMTfields,FMTstring)
    !!--..       if (iErr_fmt == -1) GoTo 998 ! End of Line | Block treating
    !!--..       if (iErr_fmt /= 0)  GoTo 999 ! input error |   errors
    !!--..       Read(unit=aLine,fmt=FMTstring) String,i1,R1,R2,i2
    !!--..       ......
    !!--..   998 Continue ! End of file encountered
    !!--..       ......
    !!--..    !-- Output error message if any
    !!--..   999 Continue
    !!--..        if(ierr_fmt /= 0 .and. Mess_FindFMT%nlines > 0) then
    !!--..          do i=1,Mess_FindFMT%nlines
    !!--..           Write(unit=lun,fmt="(a)") Mess_FindFMT%txt(i)
    !!--..          end do
    !!--..        end if
    !!--..        ........
    !!--..
    !!---- Update: January - 2009
    !!
    Module Subroutine FindFmt(IUnit,aLine,FMTfields,FMTstring,idebug)
       !---- Arguments ----!
       Integer ,           intent(in    ) ::  IUnit      ! Logical unit number
       Character (len=*) , intent(in out) ::  aLine      ! character string to be decoded
       Character (len=*) , intent(in    ) ::  FMTfields  ! description of the format fields (e.g. IIFIF)
       Character (len=*) , intent(   out) ::  FMTstring  ! format of the line (e.g. (I5,I1,F8.0,I4,F7.0,)
       Integer ,optional,  intent(in    ) ::  idebug     ! Logical unit number for writing the input file


       !---- Local variables ----!
       Character (len=len(FMTfields)) ::  UFMTfields
       Integer  :: nC_L     ! counts characters in Line
       Integer  :: ioS      ! Fortran status code
       Integer  :: L_Fields ! true length of format descriptor
       Integer  :: L_Line   ! true length of data line
       Integer  :: nCar     ! counts characters in current format field
       Integer  :: nFld     ! counts format fields in FMTfields
       Integer  :: nStr     ! counts characters in FMTstring
       Integer  :: iFld     ! field type -1:integer;-2:real;>0:A1 to A14
       Integer  :: GetFTMfield     ! old function now argument of a subroutine
       Logical  :: ifSearchEnd

       !---- Initialize ----!
       nC_L = 0
       nFld = 0
       FMTstring = "()"     ! will receive FORTRAN format
       nStr = 1             ! at least a right parentheses in FMTstring
       Err_CFML%Ierr = iErrNone
       L_Fields  = Len_trim(FMTfields)
       line_nb = line_nb + 1  ! Update the line number
       !---- Format descriptor in upper case ----!
       if (FMTfields == " ") then
          Err_CFML%Ierr = iErrFields           ! empty FMT format descriptor
          Call FindFMT_Err(aLine,nC_L)
          Err_CFML%nl=Err_CFML%nl+1
          Write(unit=Err_CFML%txt(Err_CFML%nl),fmt="(a,i6,a)")    &
               " => Please check your input file at line: ",Line_Nb," !"
               return
       end if
       UFMTfields=u_case(FMTfields)

       !---- (Get and) verify data line ----!
       if (iunit > 0) then
          do
             Read(unit=iunit,fmt="(a)",ioStat=ioS) aLine
             if (ioS == -1) then
                Err_CFML%Ierr = iErrEof            ! End Of File
                Err_CFML%nl=Err_CFML%nl+1
                Write(unit=Err_CFML%txt(Err_CFML%nl),fmt="(a,i4)") " => Non FATAL End of file !,  logical unit: ",iunit
                return                    !leave reading routine to handle end of file

             else if (ioS > 0) then
                Err_CFML%Ierr = -ioS-100           ! FORTRAN read error
                Call FindFMT_Err(aLine,nC_L)
                Err_CFML%nl=Err_CFML%nl+1
                Write(unit=Err_CFML%txt(Err_CFML%nl),fmt="(a,i6,a)")    &
                     " => Please check your input file at line: ",Line_Nb," !"
                return
             end if
             aLine=adjustl(aLine)
             l_line = len_trim(aLine)    ! true length without trailing spaces
             if(present(idebug) .and. idebug > 0) write(unit=idebug,fmt="(a)") trim(aLine)
             if (aLine(1:1) == "!" .or. aLine(1:1) == "#" .or. L_line == 0) then
                Line_Nb=Line_Nb+1
             else
                exit
             end if
          end do
       else
          l_line = len_trim(aLine)
       end if

       !---- Start decoding line character by character ----!
       ifSearchEnd = .false.

       do
          if (ifSearchEnd) exit

          !---- Get a new format field type ----!
          nCar = 0                    ! new format field
          call SGetFTMfield(GetFTMfield,UFMTfields, nFld, L_fields)
          iFld = GetFTMfield
          if (Err_CFML%Ierr /= iErrNone) then ! Error in field definition
             Call FindFMT_Err(aLine,nC_L)
             Err_CFML%nl=Err_CFML%nl+1
             Write(unit=Err_CFML%txt(Err_CFML%nl),fmt="(a,i6,a)")    &
                  " => Please check your input file at line: ",Line_Nb," !"
             return
          end if
          if (iFld == iEndFMT) then   ! format exhausted
             if (nFld == 0) then
                Err_CFML%Ierr = iErrInvalField   ! invalid field in FMTfields
                Call FindFMT_Err(aLine,nC_L)
                Err_CFML%nl=Err_CFML%nl+1
                Write(unit=Err_CFML%txt(Err_CFML%nl),fmt="(a,i6,a)")    &
                     " => Please check your input file at line: ",Line_Nb," !"
                return
             else
                exit                    ! scan end
             end if
          end if

          !---- Decode current field (character or numeric ?) ----!
          if (iFld > iEndFMT) then
             Call TreatMCharField(iFld,aLine,L_Line,nC_L,nCar)
          else if (iFld == iEndFMT) then    ! format exhausted
             exit
          else if (iFld < iEndFMT) then
             Call TreatNumerField(iFld,aLine,L_Line,nC_L,nCar)
          end if
          if (Err_CFML%Ierr /= iErrNone) then
             Call FindFMT_Err(aLine,nC_L)
             Err_CFML%nl=Err_CFML%nl+1
             Write(unit=Err_CFML%txt(Err_CFML%nl),fmt="(a,i6,a)")    &
                  " => Please check your input file at line: ",Line_Nb," !"
             return
          end if
          if ((iFld < iEndFMT .and. nCar == 0) .or. iFld == 0) then
             Err_CFML%Ierr = iErrEmptyField           ! no characters in field
             return
          end if

          !---- Build current FMT element ----!
          Call BuildFMT(iFld,nCar,nStr,FMTstring)
          if (Err_CFML%Ierr /= iErrNone) then   ! format string length exceeded
             Call FindFMT_Err(aLine,nC_L)
             Err_CFML%nl=Err_CFML%nl+1
             Write(unit=Err_CFML%txt(Err_CFML%nl),fmt="(a,i6,a)")    &
                  " => Please check your input file at line: ",Line_Nb," !"
             return
          end if

          !---- End of data Line ? ----!
          if (nC_L >= L_Line) ifSearchEnd = .true.
       end do

       !---- Terminates and close the format field ----!

       !---- If FMT not exhausted we append the remaining fields to ----!
       !---- the format string                                      ----!
       if (Err_CFML%Ierr == iErrNone .and. nFld < L_Fields) then
          !do while (iFld /= iEndFMT)
          do
             if (iFld == iEndFMT) exit
             call SGetFTMfield(GetFTMfield,UFMTfields, nFld, L_fields)
             iFld = GetFTMfield
             if (Err_CFML%Ierr /= iErrNone) then   ! Error in field definition
                Call FindFMT_Err(aLine,nC_L)
                Err_CFML%nl=Err_CFML%nl+1
                Write(unit=Err_CFML%txt(Err_CFML%nl),fmt="(a,i6,a)")    &
                     " => Please check your input file at line: ",Line_Nb," !"
                return
             end if
             if (iFld /= iEndFMT) then
                nCar=1     !Put ==1 because BuildFMT required INOUT arg.
                Call BuildFMT(iFld,nCar,nStr,FMTstring)
                if (Err_CFML%Ierr /= iErrNone) then ! format string length exceeded
                   Call FindFMT_Err(aLine,nC_L)
                   Err_CFML%nl=Err_CFML%nl+1
                   Write(unit=Err_CFML%txt(Err_CFML%nl),fmt="(a,i6,a)")    &
                        " => Please check your input file at line: ",Line_Nb," !"
                   return
                end if
             end if
          end do
       end if

       !---- Close format string ----!
       FMTstring(nStr:nStr) = ")"

    End Subroutine FindFmt

    !!--++
    !!--++ SUBROUTINE FINDFMT_ERR
    !!--++
    !!--++    (PRIVATE)
    !!--++    Output the error messages from FindFMT
    !!--++
    !!--++ Update: February - 2005
    !!
    Module Subroutine FindFMT_Err(aLine,nC_L)
       !---- Arguments ----!
       Character(len=*), intent(in) ::   aLine      ! Current data line
       Integer,         intent (in) ::   nC_L       ! location of last character treated

       !---- Local variables ----!
       Integer, parameter                             :: MssgBeg=-2   ! lower message number
       Integer, parameter                             :: MssgEnd=12   ! upper message number
       Character (len=48), dimension(MssgBeg:MssgEnd) :: Message=(/ &
                                                         "FindFMT: data line FORTRAN read error nber:     ",          &
                                                         "FindFMT: End of file !                          ",          &
                                                         "FindFMT: no error                               ",          &
                                                         "FindFMT: empty format descriptor (0 field) !    ",          &
                                                         "FindFMT: data string, read error !              ",          &
                                                         "FindFMT: integer field found real !             ",          &
                                                         "FindFMT: begged dot, sign or 'e' character !    ",          &
                                                         "FindFMT: invalid character in an integer field !",          &
                                                         "FindFMT: invalid field in format descriptor !   ",          &
                                                         "FindFMT: invalid character in a numeric field ! ",          &
                                                         "FindFMT: 0 character in current field !         ",          &
                                                         "FindFMT: format string length exceeded !        ",          &
                                                         "FindFMT: separator missing !                    ",          &
                                                         "FindFMT: incomplete E or D format !             ",          &
                                                         "FindFMT: incomplete number !                    "/)

       Integer                                         :: Ln, i
       Character (len=40)                              :: LaMarque

       !---- Error message ----!
       if (Err_CFML%Ierr == iErrNone .or. Err_CFML%Ierr == iErrEof) then
          Return
       else if (Err_CFML%Ierr < iErrEof) then
          Err_CFML%nl=1
          Write(unit=Err_CFML%txt(1),fmt="(a,i4)") " "//Message(-2)(1:Len_trim(Message(-2))), -(Err_CFML%Ierr+100)
       else if (Err_CFML%Ierr < MssgBeg .or. Err_CFML%Ierr > MssgEnd) then
          Err_CFML%nl=1
          Write(unit=Err_CFML%txt(1),fmt="(a,i2)") " FMT decode error number:",Err_CFML%Ierr
       else
          Err_CFML%nl=1
          Write(unit=Err_CFML%txt(1),fmt="(a)") " "//Message(Err_CFML%Ierr)(1:Len_trim(Message(Err_CFML%Ierr)))
       end if

       !---- Output data line and print a mark at error location ----!
       Ln = max(Len_trim(aLine),1)
       Ln = min(Ln,178)  !Prevent overflow of Err_CFML%txt that has a maximum of 180 characters
       if (Ln <= 175) then
          Err_CFML%nl=2
          Write(unit=Err_CFML%txt(2),fmt="(tr1,a)") "'"//aLine(1:Ln)//"'"
          if (nC_L == 1) then
             Err_CFML%nl=3
             Write(unit=Err_CFML%txt(3),fmt="(tr1,a)")  "  ^----"
          else if (nC_L > 1) then
             Write(unit=LaMarque,fmt="(a,i3,a)")  "(a,", nC_L, "a,a)"
             Err_CFML%nl=3
             write(unit=Err_CFML%txt(3),fmt=LaMarque)  " ",("-",i=1,nC_L),"^"
          end if
       else
          Err_CFML%nl=2
          Write(unit=Err_CFML%txt(2),fmt="(a)") " "//aLine(1:Ln)
          Write(unit=LaMarque,fmt="(a,i3,a)")  "(a,", nC_L-1, "a,a)"
          Err_CFML%nl=3
          Write(unit=Err_CFML%txt(3),fmt=LaMarque) " ",("-",i=1,nC_L-1),"^"
       end if

    End Subroutine FindFMT_Err

    !!----
    !!---- INC_LINENUM
    !!----    Increments the current line number
    !!----    Used when a way of reading other than FindFMT is used
    !!----
    !!---- Update: November - 2006
    !!
    Module Subroutine Inc_LineNum(line_n)
       !---- Argument ----!
       integer, intent(in) :: line_n

       line_nb=line_nb+line_n

    End Subroutine Inc_LineNum

    !!----
    !!---- INIT_FINDFMT
    !!----
    !!----    Initializes the subroutine FindFMT.
    !!----    Mess_FindFMT (of type Err_Text_Type) is initialized to zero lines.
    !!----    Line_nb is initialized to zero (current line in the file),
    !!----    or Line_nb=line if the optional argument "line" is present.
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine Init_FindFMT(nline)
       !---- Arguments ----!
       integer, optional, intent(in) :: nline

       line_nb=0
       if(present(nline)) line_nb=nline
       call clear_error()

    End Subroutine Init_FindFMT

    !!--++
    !!--++ SGETFTMFIELD
    !!--++
    !!--++    (PRIVATE)
    !!--++    Get current field type
    !!--++
    !!--++ Update: February - 2005
    !!
    Module Subroutine SGetFTMfield(GetFTMfield,FMTfields,nFld,nFldMax)
       !---- Arguments ----!
       Integer ,          intent(out)    ::  GetFTMfield
       Character (len=*) ,intent( in)    ::  FMTfields        !  -> format descriptor
       Integer ,          intent(in out) ::  nFld             ! <-> current field in format descriptor
       Integer ,          intent( in)    ::  nFldMax          !  -> max. number of fields in format descriptor

       !---- Local variables ----!
       character (len=1) ::  Car

       nFld = nFld + 1
       if (nFld > nFldMax) then
          GetFTMfield = iEndFMT
       else
          Car = FMTfields(nFld:nFld)
          if (Car == "I") then
             GetFTMfield = iInte
          else if (Car == "F") then
             GetFTMfield = iReal
          else if (iChar(Car) >= i_One .and. iChar(Car) <= i_Nine) then
             GetFTMfield = iChar(Car)
          else if (Car == "A") then
             GetFTMfield = 10+i_Zero
          else if (Car == "B") then
             GetFTMfield = 11+i_Zero
          else if (Car == "C") then
             GetFTMfield = 12+i_Zero
          else if (Car == "D") then
             GetFTMfield = 13+i_Zero
          else if (Car == "E") then
             GetFTMfield = 14+i_Zero
          else
             GetFTMfield = iEndFMT
             Err_CFML%Ierr = iErrInvalField         ! Error in field definition
          end if
       end if

    End Subroutine SGetFTMfield

    !!--++
    !!--++ SUBROUTINE TREATMCHARFIELD
    !!--++
    !!--++    (PRIVATE)
    !!--++    Fixed length "A1 to A9" field : A<iFld-48>
    !!--++    Leading spaces are ignored; separators : space and Tab
    !!--++
    !!--++ Update: February - 2005
    !!
    Module Subroutine TreatMCharField(iFld,aLine,L_Line,nC_L,nC_X)
       !---- Arguments ----!
       Integer,           intent(in out)  :: iFld      ! <-> "A" format size (1 to 9)
       Character (len=*), intent(in)      :: aLine     !  -> data line to be analysed
       Integer,           intent(in)      :: L_Line    !  -> true length of data Line
       Integer,           intent(in out)  :: nC_L      ! <-> current character in data line
       Integer,           intent(out)     :: nC_X      ! <-  number of characters in X format field (now nx -> trn)

       !---- Local variables ----!
       Character (len=1) ::   Car
       Integer           ::   nCar
       Logical           ::   ifEnd

       nC_X = 0
       Err_CFML%Ierr = 0

       !---- End of ligne ----!
       if (nC_L >= L_Line) return

       !---- if not 1rst field, 1rst character must be a separator ----!
       if (nC_L > 1) Then
          nC_L = nC_L+1
          Car  = aLine(nC_L:nC_L)
          if (Car /= " " .and. Car /= cTab) then
             Err_CFML%Ierr = iErrSepMiss              ! separator missing
             return
          end if
          nC_X = nC_X+1
       end if

       !---- Remove leading spaces ----!
       ifEnd = .false.
       do
          if (ifEnd) exit
          if (nC_L >= L_Line) return        ! end of line
          nC_L = nC_L+1
          Car  = aLine(nC_L:nC_L)
          if (Car == " ") then
             nC_X = nC_X+1                   ! count leading spaces
          else
             ifEnd = .true.                  ! 1rst valid character
             nC_L = nC_L-1
          end if
       end do

       !---- Count characters until next Tab or end of line ----!
       nCar = 0
       ifEnd = .false.
       do
          if (ifEnd) exit
          if (nC_L < L_Line .and. nCar < (iFld-48)) then
             nC_L = nC_L+1
             nCar = nCar+1
             Car = aLine(nC_L:nC_L)
             if (Car == " " .or. Car == cTab) then
                ifEnd = .true.                ! separator found
                nCar  = nCar - 1
                nC_L  = nC_L - 1
             end if
          else
             ifEnd = .true.                  ! end of line
          end if
       end do

       !---- Load size of the A format field ----!
       if (nCar == 0) then
          Err_CFML%Ierr = iErrEmptyField             ! no charac. in field
       else
          iFld = nCar+48                    ! true size of the A field
       end if

    End Subroutine TreatMCharField

    !!--++
    !!--++ SUBROUTINE TREATNUMERFIELD
    !!--++
    !!--++    (PRIVATE)
    !!--++    Free "I" and "F" formats
    !!--++    Look for a separator (space or Tab) after any valid character
    !!--++
    !!--++ Update: February - 2005
    !!
    Module Subroutine TreatNumerField(iFld,aLine,L_Line,nC_L,nCar)
       !---- Arguments ----!
       Integer ,          intent( in)    ::  iFld   ! field type
       Character (len=*), intent(in out) ::  aLine  ! data line
       Integer ,          intent( in)    ::  L_Line ! true length of the data line
       Integer ,          intent(in out) ::  nC_L   ! counts characters in data line
       Integer ,          intent(in out) ::  nCar   ! counts characters in format field

       !---- Local variables ----!
       Character (len=1)   ::  Car,Car_
       Integer             ::  nCar1                ! 1st usefull character in field
       Integer             ::  nPosi                ! number of 1st character in field
       Logical             ::  ifEnd,ifDot,ifSign

       Err_CFML%Ierr   = 0
       nCar   = 0
       ifDot  = .false.
       ifSign = .false.
       nPosi  = nC_L

       !---- Skip previous separator (space, Tab or sign) and leading spaces ----!
       ifEnd = .false.
       do
          if (ifEnd) exit
          nC_L = nC_L+1
          if (nC_L <= L_Line) then
             nCar = nCar+1
             Car = aLine(nC_L:nC_L)

             !---- Tab character ----!
             if (Car == cTab) Then
                if (nCar == 1 .and. nC_L > 1) then
                   aLine(nC_L:nC_L) = " "      ! previous separator
                else
                   if (ifSign) then
                      Err_CFML%Ierr = iErrNumber         ! incomplete number
                      return
                   end if
                   nC_L = nC_L-1               ! new separator
                   nCar = nCar-1
                   return
                end if

             else if (Car == "+" .or. Car == "-") then
                !---- a sign ----!
                ifSign = .true.

             else if (Car == " ") then
                !---- a space ----!
                if (ifSign) then
                   Err_CFML%Ierr = iErrNumber           ! incomplete number
                   return
                end if

             else
                !---- any other character ----!
                ifEnd = .true.
             end if
          else
             return                          ! end of line
          end if
       end do

       !---- No valid previous separator found (Except for 1st field) ----!
       if (nPosi > 1 .and. nCar == 1) then
          Err_CFML%Ierr = iErrSepMiss                ! separator missing
          return
       end if

       !---- Check first character and initialize search ----!

       !---- Decimal point -> valid in real fields only ----!
       if (Car == ".") then
          ifDot = .true.
          if (iFld /= iReal)  then
             Err_CFML%Ierr = iErrFieldType            ! not an integer field
             Return
          end if

       else if(Car == "E".or.Car == "e".or.Car == "d".or.Car == "D") then
          !---- e,E,d,D -> always invalid at this position ----!
          if (iFld == iReal) then
             Err_CFML%Ierr = iErrEfrmt                ! incomplete E or D format
          else
             Err_CFML%Ierr = iErrInvalC               ! invalid char in int. field
          end if
          return

       else if (iChar(Car) < i_Zero .or. iChar(Car) > i_Nine) then
          !---- invalid if not a sign or a digit ----!
          Err_CFML%Ierr = iErrInvalChar        ! invalid character
          return
       end if

       !---- save position of first character ----!
       nCar1 = nCar

       !---- Count characters in number ----!
       ifEnd = .false.

       do
          if (ifEnd) exit
          Car_ = Car      ! save previous character
          nC_L = nC_L+1
          if (nC_L <= L_Line) then
             nCar = nCar+1
             Car = aLine(nC_L:nC_L)

             !---- Current character is a decimal point ----!
             if (Car == ".") then
                if (ifDot) then
                   Err_CFML%Ierr = iErrCharBegg         ! begged character (dot)
                   Return
                else if (iFld /= iReal) then
                   Err_CFML%Ierr = iErrFieldType        ! not an integer field
                   Return
                else
                   ifDot = .true.
                end if

             else if (Car == " " .or. Car == cTab) then
                !---- Current character is a space or Tab (separator) ----!
                if (Car_ == "+" .or. Car_ == "-") then
                   Err_CFML%Ierr = iErrNumber           ! incomplete number
                   return
                end if
                ifEnd = .true.
                nCar  = nCar - 1
                nC_L  = nC_L - 1

             else if (Car == "+" .or. Car == "-") then
                !---- Current character is a sign ----!
                if (Car_ == "+" .or. Car_ == "-") then
                   Err_CFML%Ierr = iErrCharBegg         ! begged character
                   return
                else if (nCar > nCar1) then
                   if (iFld == iReal) then
                      if (Car_ /= "E" .and. Car_ /= "e" .and. Car_ /= "D" .and. Car_ /= "d") then
                         ifEnd = .true.          ! Sign is a valid separator
                         nCar  = nCar - 1
                         nC_L  = nC_L - 1
                         Return
                      end if
                   else                        ! Sign is a valid separator
                      ifEnd = .true.
                      nCar  = nCar - 1
                      nC_L  = nC_L - 1
                      Return
                   end if
                end if

             else if (Car == "E" .or. Car == "e" .or. Car == "d" .or. Car == "D") then
                !---- Current character is a "e E d D" ----!
                if (nCar == nCar1 .or. Car_ == "-" .or. Car_ == "+") then
                   Err_CFML%Ierr = iErrEfrmt            ! incomplete E or D format
                   return
                else if (Car_ == Car) then
                   Err_CFML%Ierr = iErrCharBegg         ! begged character
                   return
                end if

             else if (iChar(Car) < i_Zero .or. iChar(Car) > i_Nine) then
                !---- Ccurrent character is not a valid one ? ----!
                Err_CFML%Ierr = iErrInvalChar          ! invalid character
                Return
             end if
          else
             ifEnd = .true.                  ! end of line
          end if
       end do

    End Subroutine TreatNumerField

End Submodule StringFullp
