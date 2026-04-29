Submodule (CFML_Keywords_Code_Parser) KWC_SplitOperations
   !---- Variables ----!
   implicit none

 Contains

    !!--++
    !!--++ Module Subroutine Split_Operations(Line, Ni, S_Lines)
    !!--++    character(len=*),              intent( in) :: line
    !!--++    integer,                       intent(out) :: ni
    !!--++    character(len=*),dimension(:), intent(out) :: s_lines
    !!--++
    !!--++    (Private)
    !!--++    Split diferent directives according to KEY_CODE Variable
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Split_Operations(Line, Ni, S_Lines)
       !---- Arguments ----!
       character(len=*),              intent( in) :: line
       integer,                       intent(out) :: ni
       character(len=*),dimension(:), intent(out) :: s_lines

       !---- Local variables ----!
       character(len=150)      :: linec
       character(len=10)       :: car
       integer                 :: i,j,npos
       integer,dimension(nkey) :: ip,ipc

       ni=0
       s_lines=" "

       linec=u_case(line)

       !---- Search Subkeys: XYZ,OCC,BIS... ----!
       ip=0
       do i=1,nkey
          npos=index(linec,key_code(i))
          if (npos > 0) then
             car=linec(npos:)
             j=index(car," ")
             if (j > 0) car=car(:j-1)
             j=index(car,"_")
             if (j > 0) cycle
          end if
          ip(i)=npos
       end do

       npos=count(ip > 0)
       if (npos == 0) then
          ni=1
          s_lines(1)=adjustl(line)
       else
          !call sort(ip,nkey,ipc)
          ipc=sort(ip,nkey)
          do i=1,nkey
             if (ip(ipc(i)) == 0) then
                if (ip(ipc(i+1)) <= 1) cycle
                ni=ni+1
                s_lines(ni)=adjustl(line(1:ip(ipc(i+1))-1))
             else
                ni=ni+1
                if (i < nkey) then
                   s_lines(ni)=adjustl(line(ip(ipc(i)):ip(ipc(i+1))-1))
                else
                   s_lines(ni)=adjustl(line(ip(ipc(i)):))
                end if
             end if
          end do
       end if
    End Subroutine Split_Operations

    !!----
    !!---- Module Subroutine Split_GOperations(Line, Ni, S_Lines)
    !!----    character(len=*),              intent( in) :: line
    !!----    integer,                       intent(out) :: ni
    !!----    character(len=*),dimension(:), intent(out) :: s_lines
    !!----
    !!----    (Private)
    !!----    Split diferent directives according to GCODE_NAM Variable
    !!----
    !!---- Update: November - 2013
    !!
    Module Subroutine Split_GOperations(Line, Ni, S_Lines)
       !---- Arguments ----!
       character(len=*),              intent( in) :: line
       integer,                       intent(out) :: ni
       character(len=*),dimension(:), intent(out) :: s_lines

       !---- Local variables ----!
       character(len=150)        :: linec
       character(len=10)         :: up_gcode_nam
       integer                   :: i,npos
       integer,dimension(NGCode) :: ip,ipc

       ni=0
       s_lines=" "

       linec=u_case(line)

       !---- Search Subkeys: scale,cell,delta... ----!
       ip=0
       do i=1,NGCode
          up_gcode_nam=u_case(Gcode_Nam(i))
          ip(i)=index(linec,up_gcode_nam)
       end do

       npos=count(ip > 0)
       if (npos == 0) then
          ni=1
          s_lines(1)=adjustl(line)
       else
          !call sort(ip,NGCode,ipc)
          ipc=sort(ip,NGCode)
          do i=1,NGCode
             if (ip(ipc(i)) == 0) then
                if (ip(ipc(i+1)) <= 1) cycle
                ni=ni+1
                s_lines(ni)=adjustl(line(1:ip(ipc(i+1))-1))
             else
                ni=ni+1
                if (i < NGCode) then
                   s_lines(ni)=adjustl(line(ip(ipc(i)):ip(ipc(i+1))-1))
                else
                   s_lines(ni)=adjustl(line(ip(ipc(i)):))
                end if
             end if
          end do
       end if
    End Subroutine Split_GOperations

    !!--++
    !!--++ Module Subroutine Split_mOperations(Line, Ni, S_Lines)
    !!--++    character(len=*),              intent( in) :: line
    !!--++    integer,                       intent(out) :: ni
    !!--++    character(len=*),dimension(:), intent(out) :: s_lines
    !!--++
    !!--++    (Private)
    !!--++    Split diferent directives according to KEY_mCODE Variable
    !!--++    magnetic clone of Subroutine Split_Operations
    !!--++ Created: December - 2011
    !!
    Module Subroutine Split_mOperations(Line, Ni, S_Lines)
       !---- Arguments ----!
       character(len=*),              intent( in) :: line
       integer,                       intent(out) :: ni
       character(len=*),dimension(:), intent(out) :: s_lines

       !---- Local variables ----!
       character(len=150)      :: linec
       character(len=10)       :: car
       integer                 :: i,j,npos
       integer,dimension(mnkey) :: ip,ipc

       ni=0
       s_lines=" "

       linec=u_case(line)

       !---- Search Subkeys: Rxyz,Ixyz,Mxyz ----!
       ip=0
       do i=1,mnkey
          npos=index(linec,key_mcode(i))
          if (npos > 0) then
             car=linec(npos:)
             j=index(car," ")
             if (j > 0) car=car(:j-1)
             j=index(car,"_")
             if (j > 0) cycle
          end if
          ip(i)=npos
       end do

       npos=count(ip > 0)
       if (npos == 0) then
          ni=1
          s_lines(1)=adjustl(line)
       else
          !call sort(ip,mnkey,ipc)
          ipc=sort(ip,mnkey)
          do i=1,mnkey
             if (ip(ipc(i)) == 0) then
                if (ip(ipc(i+1)) <= 1) cycle
                ni=ni+1
                s_lines(ni)=adjustl(line(1:ip(ipc(i+1))-1))
             else
                ni=ni+1
                if (i < mnkey) then
                   s_lines(ni)=adjustl(line(ip(ipc(i)):ip(ipc(i+1))-1))
                else
                   s_lines(ni)=adjustl(line(ip(ipc(i)):))
                end if
             end if
          end do
       end if
    End Subroutine Split_mOperations

End Submodule KWC_SplitOperations
