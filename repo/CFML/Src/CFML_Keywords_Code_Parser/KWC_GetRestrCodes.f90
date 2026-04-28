Submodule (CFML_Keywords_Code_Parser) KWC_GetRestrCodes
   !---- Variables ----!
   implicit none

 Contains

    !!----
    !!---- Module Subroutine Get_RestAng_Line(Line, FAtom)
    !!----    character(len=*),        intent(in)     :: Line
    !!----    type(AtList_Type),       intent(in out) :: FAtom
    !!----
    !!----     Get Distance Restraints relations for Free atoms type
    !!----     Line: Angle [sig] At1a At1b At1c At2a At2b At2c....
    !!----
    !!---- Update: March - 2005
    !!
    Module Subroutine Get_RestAng_Line(Line, FAtom)
       !---- Arguments ----!
       character(len=*),        intent(in) :: Line
       type(AtList_Type),       intent(in) :: FAtom

       !---- Local variables ----!
       integer, parameter                :: np=30
       character(len=30),dimension(np)   :: dire
       character(len=8), dimension(2,np) :: symtrans
       integer,dimension(3,np)           :: p
       real                              :: ang,sig

       character(len=8), dimension(2)  :: car
       integer                         :: i,j,iv,nc,nr,n_ini,n_end,npos
       integer, dimension(3)           :: ivet
       real(kind=cp), dimension(3)     :: vet


       if (len_trim(line) == 0 .or. .not. allocated(ang_rest)) return

       !---- Description for each word ----!
       call Get_Words(line,dire,nc)

       !---- Get Angle ----!
       call Get_Num(dire(1),vet,ivet,iv)
       if (iv /= 1) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="Error in AFIX line: "//trim(line)
          return
       end if
       ang=vet(1)

       !---- Get Sigma ----!
       call Get_Num(dire(2),vet,ivet,iv)
       if (iv /= 1) then
          sig=0.2
          n_ini=2
       else
          sig=max(vet(1),0.001_cp)
          n_ini=3
       end if

       nr=0
       symtrans=" "
       do i=n_ini,nc,3
          ivet=0
          car=" "
          npos=index(dire(i),"_")
          if (npos /=0) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg=" The first atom in AFIX command must belong to the asymmetric unit: "//trim(Line)
             return
          end if
          npos=index(dire(i+1),"_")
          if (npos /=0) then
             car(1)=dire(i+1)(npos:)
             dire(i+1)=dire(i+1)(1:npos-1)
          end if
          npos=index(dire(i+2),"_")
          if (npos /=0) then
             car(2)=dire(i+2)(npos:)
             dire(i+2)=dire(i+2)(1:npos-1)
          end if

          do j=1,FAtom%natoms
             if (trim(u_case(dire(i))) == trim(u_case(FAtom%Atom(j)%Lab))) then
                ivet(1)=j
             end if
             if (trim(u_case(dire(i+1))) == trim(u_case(FAtom%Atom(j)%Lab))) then
                ivet(2)=j
             end if
             if (trim(u_case(dire(i+2))) == trim(u_case(FAtom%Atom(j)%Lab))) then
                ivet(3)=j
             end if
             if (all(ivet > 0) ) exit
          end do
          if (any(ivet == 0)) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="  Some atom names in "//trim(line)//" not found in the asymmetric unit"
             return
          end if

          !---- New Relation ----!
          nr=nr+1
          p(:,nr)=ivet
          symtrans(:,nr)=car
       end do
       if (nr <= 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="Illegal AFIX command  "//trim(line)
          return
       end if

       !---- Adding relations ----!
       n_ini=np_rest_ang+1
       n_end=np_rest_ang+nr
       ang_rest(n_ini:n_end)%aobs=ang
       ang_rest(n_ini:n_end)%acalc=0.0
       ang_rest(n_ini:n_end)%sigma=sig
       ang_rest(n_ini:n_end)%p(1) = p(1,1:nr)
       ang_rest(n_ini:n_end)%p(2) = p(2,1:nr)
       ang_rest(n_ini:n_end)%p(3) = p(3,1:nr)
       ang_rest(n_ini:n_end)%STCode(1)=symtrans(1,1:nr)
       ang_rest(n_ini:n_end)%STCode(2)=symtrans(2,1:nr)
       np_rest_ang=n_end
    End Subroutine Get_RestAng_Line

    !!----
    !!---- Module Subroutine Get_RestDis_Line(Line, FAtom)
    !!----    character(len=*),        intent(in)     :: Line
    !!----    type(AtList_Type),       intent(in out) :: FAtom
    !!----
    !!----    Get Distance Restraints relations for Free atoms type
    !!----    Line: Dist [sig] At1a At1b At2a At2b......
    !!----
    !!---- Update: March - 2005
    !!
    Module Subroutine Get_RestDis_Line(Line, FAtom)
       !---- Arguments ----!
       character(len=*),        intent(in) :: Line
       type(AtList_Type),       intent(in) :: FAtom

       !---- Local variables ----!
       integer, parameter              :: np=20
       character(len=30),dimension(np) :: dire
       character(len=8), dimension(np) :: symtrans
       integer,dimension(2,np)         :: p
       real                            :: dis,sig

       character(len=8)                :: car
       integer                         :: i,j,iv,nc,nr,n_ini,n_end,npos
       integer, dimension(2)           :: ivet
       real(kind=cp), dimension(2)     :: vet


       if (len_trim(line) == 0 .or. .not. allocated(dis_rest) ) return

       !---- Description for each word ----!
       call Get_Words(line,dire,nc)

       !---- Get Dist ----!
       call Get_Num(dire(1),vet,ivet,iv)
       if (iv /= 1) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="Error in DFIX line: "//trim(line)
          return
       end if
       dis=vet(1)

       !---- Get Sigma ----!
       call Get_Num(dire(2),vet,ivet,iv)
       if (iv /= 1) then
          sig=0.02
          n_ini=2
       else
          sig=max(vet(1),0.0001_cp)
          n_ini=3
       end if

       nr=0
       symtrans=" "
       do i=n_ini,nc,2
          ivet=0
          car=" "
          npos=index(dire(i),"_")
          if (npos /=0) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg=" The first atom in DFIX command must belong to the asymmetric unit: "//trim(Line)
             return
          end if
          npos=index(dire(i+1),"_")
          if (npos /=0) then
             car=dire(i+1)(npos:)
             dire(i+1)=dire(i+1)(1:npos-1)
          end if

          do j=1,FAtom%natoms
             if (trim(u_case(dire(i))) == trim(u_case(FAtom%Atom(j)%Lab))) then
                ivet(1)=j
             end if
             if (trim(u_case(dire(i+1))) == trim(u_case(FAtom%Atom(j)%Lab))) then
                ivet(2)=j
             end if
             if (all(ivet > 0) ) exit
          end do
          if (any(ivet == 0)) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="  Some atom names in"//trim(line)//" not found in the asymmetric unit"
             return
          end if

          !---- New Relation ----!
          nr=nr+1
          p(:,nr)=ivet
          symtrans(nr)=car
       end do
       if (nr <= 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="Illegal DFIX command  "//trim(line)
          return
       end if

       !---- Adding relations ----!
       n_ini=np_rest_dis+1
       n_end=np_rest_dis+nr
       dis_rest(n_ini:n_end)%dobs=dis
       dis_rest(n_ini:n_end)%dcalc=0.0
       dis_rest(n_ini:n_end)%sigma=sig
       dis_rest(n_ini:n_end)%p(1) = p(1,1:nr)
       dis_rest(n_ini:n_end)%p(2) = p(2,1:nr)
       dis_rest(n_ini:n_end)%STCode=symtrans(1:nr)
       np_rest_dis=n_end
    End Subroutine Get_RestDis_Line

    !!----
    !!---- Module Subroutine Get_RestTor_Line(Line, FAtom)
    !!----    character(len=*),        intent(in)     :: Line
    !!----    type(AtList_Type),       intent(in out) :: FAtom
    !!----
    !!----    Get Torsion Restraints relations for Free atoms type
    !!----    Line: Torsion_Angle [sig] At1a At1b At1c At1d ...
    !!----
    !!---- Update: March - 2005
    !!
    Module Subroutine Get_RestTor_Line(Line, FAtom)
       !---- Arguments ----!
       character(len=*),        intent(in) :: Line
       type(AtList_Type),       intent(in) :: FAtom

       !---- Local variables ----!
       integer, parameter                :: np=30
       character(len=30),dimension(np)   :: dire
       character(len=8), dimension(3,np) :: symtrans
       integer,dimension(4,np)           :: p
       real                              :: tor,sig

       character(len=8), dimension(3)  :: car
       integer                         :: i,j,iv,nc,nr,n_ini,n_end,npos
       integer, dimension(4)           :: ivet
       real(kind=cp), dimension(4)     :: vet


       if (len_trim(line) == 0 .or. .not. allocated(tor_rest)) return

       !---- Description for each word ----!
       call Get_Words(line,dire,nc)

       !---- Get Angle ----!
       call Get_Num(dire(1),vet,ivet,iv)
       if (iv /= 1) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="Error in TFIX line: "//trim(line)
          return
       end if
       tor=vet(1)

       !---- Get Sigma ----!
       call Get_Num(dire(2),vet,ivet,iv)
       if (iv /= 1) then
          sig=0.5
          n_ini=2
       else
          sig=max(vet(1),0.02_cp)
          n_ini=3
       end if

       nr=0
       symtrans=" "
       do i=n_ini,nc,4
          ivet=0
          car=" "
          npos=index(dire(i),"_")
          if (npos /=0) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg=" The first atom in TFIX must belong to the asymmetric unit: "//trim(Line)
             return
          end if
          npos=index(dire(i+1),"_")
          if (npos /=0) then
             car(1)=dire(i+1)(npos:)
             dire(i+1)=dire(i+1)(1:npos-1)
          end if
          npos=index(dire(i+2),"_")
          if (npos /=0) then
             car(2)=dire(i+2)(npos:)
             dire(i+2)=dire(i+2)(1:npos-1)
          end if
          npos=index(dire(i+3),"_")
          if (npos /=0) then
             car(3)=dire(i+3)(npos:)
             dire(i+3)=dire(i+3)(1:npos-1)
          end if
          do j=1,FAtom%natoms
             if (trim(u_case(dire(i))) == trim(u_case(FAtom%Atom(j)%Lab))) then
                ivet(1)=j
             end if
             if (trim(u_case(dire(i+1))) == trim(u_case(FAtom%Atom(j)%Lab))) then
                ivet(2)=j
             end if
             if (trim(u_case(dire(i+2))) == trim(u_case(FAtom%Atom(j)%Lab))) then
                ivet(3)=j
             end if
             if (trim(u_case(dire(i+3))) == trim(u_case(FAtom%Atom(j)%Lab))) then
                ivet(4)=j
             end if
             if (all(ivet > 0) ) exit
          end do
          if (any(ivet == 0)) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="  Some atom names in"//trim(line)//" not found in the asymmetric unit"
             return
          end if

          !---- New Relation ----!
          nr=nr+1
          p(:,nr)=ivet
          symtrans(:,nr)=car
       end do
       if (nr <= 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="Illegal TFIX command  "//trim(line)
          return
       end if

       !---- Adding relations ----!
       n_ini=np_rest_tor+1
       n_end=np_rest_tor+nr
       tor_rest(n_ini:n_end)%tobs=tor
       tor_rest(n_ini:n_end)%tcalc=0.0
       tor_rest(n_ini:n_end)%sigma=sig
       tor_rest(n_ini:n_end)%p(1) = p(1,1:nr)
       tor_rest(n_ini:n_end)%p(2) = p(2,1:nr)
       tor_rest(n_ini:n_end)%p(3) = p(3,1:nr)
       tor_rest(n_ini:n_end)%p(4) = p(4,1:nr)
       tor_rest(n_ini:n_end)%STCode(1)=symtrans(1,1:nr)
       tor_rest(n_ini:n_end)%STCode(2)=symtrans(2,1:nr)
       tor_rest(n_ini:n_end)%STCode(3)=symtrans(3,1:nr)
       np_rest_tor=n_end
    End Subroutine Get_RestTor_Line

End Submodule KWC_GetRestrCodes
