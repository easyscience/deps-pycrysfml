  Module DataRed_rnw_Reflections_Mod

    Use CFML_GlobalDeps
    Use CFML_Rational
    Use CFML_Maths,   only: zbelong,epss,sort,set_eps_math
    Use CFML_Metrics, only: Cell_Type, Cell_G_Type
    Use CFML_Strings, only: number_lines, FindFmt
    Use CFML_gSpaceGroups
    Use CFML_Reflections
    Use CFML_Propagation_Vectors
    Use Datared_Mod
    Use Twin_Mod

    Implicit None
    Private

    Public :: Read_Reflections_File, Write_Reflections

    contains

    Subroutine Read_Reflections_File(filhkl,cond,kinfo,Cell,Ref,Gk)
      character(len=*),                        intent(in)     :: filhkl
      type(Conditions_Type),                   intent(in out) :: cond
      type(kvect_Info_Type),                   intent(in)     :: kinfo
      type(Cell_G_type),                       intent(in)     :: Cell
      type(Reflection_List),                   intent(out)    :: Ref
      type(Group_k_Type),optional,dimension(:),intent(in out) :: Gk
      !local variables
      integer                                :: i,j,nr,inp,ier,nkv,ivk,i_rem,irm !,numor
      real(kind=cp)                          :: wavel
      character(len=132)                     :: line
      character(len=6)                       :: keyw
      character(len=80)                      :: kfmt
      integer,             dimension(3)      :: hkl
      real(kind=cp),       dimension(3)      :: h1,h2,h3
      type(ObsRef), dimension(:),allocatable :: Ob
      integer,      dimension(:),allocatable :: ind !,pnf points to not found integer indices
      logical :: found

      nr=Number_lines(trim(filhkl))+1
      allocate(Ob(nr))
      do i=1,nr
         allocate(Ob(i)%h(3+kinfo%nk))
      end do

      Open(newunit=inp,file=trim(filhkl),status="old",action="read",position="rewind")

      call clear_error()

      nr=0

      Select Case(cond%hkl_type)

        Case(0)       !Shelx-like input file (3i4,2f8.2)

            if(.not. cond%cell_given) then
              Err_CFML%Msg=" => UNIT CELL not GIVEN! Modify your input file."
              Err_CFML%Ierr=1
              return
            end if
            if(.not. cond%wave_given) then
              Err_CFML%Msg=" => WAVELENGTH not GIVEN! Modify your input file."
              Err_CFML%Ierr=1
              return
            end if
            cond%forma="(3i4,2f8.2)"
            do
               nr=nr+1
               read(unit=inp,fmt="(3i4,2f8.2)",iostat=ier)  Ob(nr)%h, Ob(nr)%intens, Ob(nr)%sigma
               if(ier /= 0 .or. (Ob(nr)%h(1)==0 .and. Ob(nr)%h(2)==0 .and. Ob(nr)%h(3) == 0) ) then
                nr=nr-1
                exit
               end if
               Ob(nr)%s=h_s(Ob(nr)%h,cell)
               h1(:)=real(Ob(nr)%h(:))
               if(cond%transf_ind) then
                 h2=matmul(cond%transhkl,h1)
               else
                 h2=h1
               end if
               Ob(nr)%h=nint(h2)
               Ob(nr)%hr=Ob(nr)%h
            end do

        Case(10)       !Shelx-like HKLF5 input file (3i4,2f8.2,i4)

             if(.not. cond%cell_given) then
              Err_CFML%Msg=" => UNIT CELL not GIVEN! Modify your input file."
              Err_CFML%Ierr=1
              return
            end if
            if(.not. cond%wave_given) then
              Err_CFML%Msg=" => WAVELENGTH not GIVEN! Modify your input file."
              Err_CFML%Ierr=1
              return
            end if

            cond%forma="(3i4,2f8.2,i4)"
            do
               nr=nr+1
               read(unit=inp,fmt="(3i4,2f8.2,i4)",iostat=ier)  Ob(nr)%h, Ob(nr)%intens, Ob(nr)%sigma, Ob(nr)%idomain
               if(ier /= 0 .or. (Ob(nr)%h(1)==0 .and. Ob(nr)%h(2)==0 .and. Ob(nr)%h(3) == 0) ) then
                nr=nr-1
                exit
               end if
               h1(:)=real(Ob(nr)%h(:))
               if(cond%transf_ind) then
                 h2=matmul(cond%transhkl,h1)
               else
                 h2=h1
               end if
               Ob(nr)%h=nint(h2)
               Ob(nr)%hr=Ob(nr)%h
            end do

        Case(11,12)       !Shelx-like HKLF5 input file (xi4,2f8.2,i4) for superspace, or free format superspace

             if(.not. cond%cell_given) then
              Err_CFML%Msg=" => UNIT CELL not GIVEN! Modify your input file."
              Err_CFML%Ierr=1
              return
            end if
            if(.not. cond%wave_given) then
              Err_CFML%Msg=" => WAVELENGTH not GIVEN! Modify your input file."
              Err_CFML%Ierr=1
              return
            end if
            cond%forma="( i4,2f8.2,i4)"
            write(unit=cond%forma(2:2),fmt="(i1)") 3+kinfo%nk
            if(cond%hkl_type == 12) cond%forma="*"  !Reading with this format does not work (this is the reason of the "if" within the loop)
            do
               nr=nr+1
               if(cond%hkl_type == 11) then
                 read(unit=inp,fmt=trim(cond%forma),iostat=ier)  Ob(nr)%h, Ob(nr)%intens, Ob(nr)%sigma, Ob(nr)%idomain
               else
                 read(unit=inp,fmt=*,iostat=ier)  Ob(nr)%h, Ob(nr)%intens, Ob(nr)%sigma, Ob(nr)%idomain
               end if
               if(ier /= 0)  then
                nr=nr-1
                exit
               end if
               if(sum(abs(Ob(nr)%h)) == 0) then
                nr=nr-1
                exit
               end if
               if(Ob(nr)%idomain == 0) Ob(nr)%idomain =1
               if(cond%transf_ind) then
                 Ob(nr)%h=nint(matmul(cond%transhkl,Ob(nr)%h))
               end if
               Ob(nr)%hr=Ob(nr)%h(1:3)
               do j=1,kinfo%nk
                   Ob(nr)%hr=Ob(nr)%hr+Ob(nr)%h(3+j)*kinfo%kv(:,j)
               end do
            end do

        Case(1)           !Free format  h,k,l,int,sigma (h,k,l real)

            if(.not. cond%cell_given) then
              Err_CFML%Msg=" => UNIT CELL not GIVEN! Modify your input file."
              Err_CFML%Ierr=1
              return
            end if
            if(.not. cond%wave_given) then
              Err_CFML%Msg=" => WAVELENGTH not GIVEN! Modify your input file."
              Err_CFML%Ierr=1
              return
            end if

            cond%forma="*"
            do
               nr=nr+1
               read(unit=inp,fmt=*,iostat=ier) h1, Ob(nr)%intens, Ob(nr)%sigma
               if(ier /= 0) then
                 nr=nr-1
                 exit
               end if
               if(.not. zbelong(h1(:))) then
                  if(cond%prop .or. cond%to_og) then
                    cond%hkl_real=.true.
                  else
                    if(cond%transf_ind) then
                       h1=matmul(cond%transhkl,h1)
                       if(.not. zbelong(h1(:))) then
                         write(unit=*,fmt="(a,3f8.3)")" => Removed reflection (non-integer): ",h1(:)
                         nr= nr-1
                         cycle
                       end if
                    else
                       write(unit=*,fmt="(a,3f8.3)")" => Removed reflection (non-integer): ",h1(:)
                       nr= nr-1
                       cycle
                    end if
                  end if
               else
                  if(cond%transf_ind) then
                     h1=matmul(cond%transhkl,h1)
                  else
                     h1=real(nint(h1))
                  end if
               end if
               Ob(nr)%hr=h1 !This is not transformed when cond%to_og=.true.
            end do

        Case(2)     ! Jane's format for .fsq:   Two lines per reflection

            if(.not. cond%cell_given) then
              Err_CFML%Msg=" => UNIT CELL not GIVEN! Modify your input file."
              Err_CFML%Ierr=1
              return
            end if
            if(.not. cond%wave_given) then
              Err_CFML%Msg=" => WAVELENGTH not GIVEN! Modify your input file."
              Err_CFML%Ierr=1
              return
            end if

            line="(i6,3f7.3,2f10.2,3f8.2)"
            cond%forma="(i6,3f7.3,2f10.2,3f8.2)"
            do
               nr=nr+1
               read(unit=inp,fmt=line,iostat=ier) &
                   Ob(nr)%numor,h1, Ob(nr)%intens, Ob(nr)%sigma, Ob(nr)%omega, Ob(nr)%chi, Ob(nr)%phi
               if(ier /= 0) then
                 nr=nr-1
                 exit
               end if
               if(.not. zbelong(h1)) then
                 if(cond%prop .or. cond%to_og) then
                   cond%hkl_real=.true.
                 else
                   if(cond%transf_ind) then
                      h1=matmul(cond%transhkl,h1)
                   else
                      write(unit=*,fmt="(a,3f8.3)")" => Removed reflection (non-integer): ",h1
                      nr= nr-1
                      cycle
                   end if
                 end if
               else
                   if(cond%transf_ind) then
                      h1=matmul(cond%transhkl,h1)
                   else
                      h1=real(nint(h1))
                   end if
               end if
               Ob(nr)%hr=h1
               read(unit=inp,fmt="(a)",iostat=ier) keyw
               if(ier /= 0) exit
            end do

        Case(3)                     !FullProf format

            read(unit=inp,fmt="(a)") line
            read(unit=inp,fmt="(a)") line
            read(unit=inp,fmt=*) wavel

            cond%forma=line

            do
               nr=nr+1
               read(unit=inp,fmt=line,iostat=ier) &
                    hkl(:), Ob(nr)%intens, Ob(nr)%sigma,i,Ob(nr)%twtheta, Ob(nr)%omega, Ob(nr)%chi, Ob(nr)%phi
               if(ier /= 0) then
                 nr=nr-1
                 exit
               end if
               h1=hkl
               if(cond%transf_ind) then
                  h1=matmul(cond%transhkl,h1)
               end if
               Ob(nr)%hr=h1
               Ob(nr)%h =hkl
            end do

        Case(4)                  !COLL 5 hkl integer

            cond%forma="(i6,3i4,2f10.2,4f8.2)"
            do
               nr=nr+1
               read(unit=inp,fmt="(i6,3i4,2f10.2,4f8.2)",iostat=ier) &
                    Ob(nr)%numor, Ob(nr)%h, Ob(nr)%intens, Ob(nr)%sigma,Ob(nr)%twtheta, Ob(nr)%omega, Ob(nr)%chi, Ob(nr)%phi
               if(ier /= 0 .or. Ob(nr)%numor == 0) then
                 nr=nr-1
                 exit
               end if
               Ob(nr)%hr=Ob(nr)%h
            end do

        Case(5) !COLL 5 hkl real numbers (old and new format with autodetection)

            read(unit=inp,fmt="(a)" ,iostat=ier) line  !Detect one of the two formats

            if(line(10:10) == ".") then  !If this position is . the old format is active
               if(line(16:16) == ".") then
                 cond%forma="(i6,3f6.2,f8.0,f4.0,4f8.2)"
               else
                 cond%forma="(i6,3f7.3,2f10.2,4f8.2)"
               end if
            else ! autodetection of the new format
               !a = index(line, ".")
               !b = index(line(a+1:), ".")
               !line=" "
               !if (b < 10) then
               !  write(line,"(a,i1,a,i1,a)") '(i6,3f', b, '.', b+6-a, ',2f10.2,4f8.2)'
               !else
               !  write(line,"(a,i2,a,i1,a)") '(i6,3f', b, '.', b+6-a, ',2f10.2,4f8.2)'
               !end if
               !cond%forma=trim(line)
               call FindFmt(0,line,"iffffffffff",kfmt)
               cond%forma=trim(kfmt)
               write(unit=*,fmt=*)"=> Will use format ",cond%forma
            end if
            rewind(unit=inp)

            irm=0
            do
               nr=nr+1
               read(unit=inp,fmt=cond%forma ,iostat=ier) &
                    Ob(nr)%numor,  Ob(nr)%hr, Ob(nr)%intens, Ob(nr)%sigma,Ob(nr)%twtheta, Ob(nr)%omega, Ob(nr)%chi, Ob(nr)%phi
               if(ier /= 0) then
                 nr=nr-1
                 exit
               end if
               h1=Ob(nr)%hr
               if(cond%transf_ind) then
                  h1=matmul(cond%transhkl,h1)
                  if(.not. zbelong(h1)) then
                    irm=irm+1
                    if(irm == 1) Open(newunit=i_rem,file="removed_reflections.rmv",status="unknown",action="write")
                    write(unit=i_rem,fmt=cond%forma) Ob(nr)%numor,  h1, Ob(nr)%intens, Ob(nr)%sigma,Ob(nr)%twtheta, Ob(nr)%omega, Ob(nr)%chi, Ob(nr)%phi
                    write(unit=*,fmt="(a,3f8.3)")" => Removed reflection (non-integer): ",h1
                    nr= nr-1
                    cycle
                  end if
               end if

               if(.not. zbelong(h1)) then
                 if(cond%prop .or. cond%to_og) then
                   cond%hkl_real=.true.
                 end if
               else
                    h1=real(nint(h1))
               end if
               Ob(nr)%hr=h1
            end do

        Case(6)    !COLL 5 hkl real numbers - 6T2 ?  Only difference with 4: i4 instead of i6 for the numor
                   !and 3f6.2 instead of 3i4 for reading the hkl-indices
            cond%forma="(i4,3f6.2,2f10.2,4f8.2)"
            do
               nr=nr+1
               read(unit=inp,fmt="(i4,3f6.2,2f10.2,4f8.2)" ,iostat=ier) &
                    Ob(nr)%numor, h1, Ob(nr)%intens, Ob(nr)%sigma,Ob(nr)%twtheta, Ob(nr)%omega, Ob(nr)%chi, Ob(nr)%phi
               if(ier /= 0) then
                 nr=nr-1
                 exit
               end if
               if(.not. zbelong(h1)) then
                 if(cond%prop .or. cond%to_og) then
                   cond%hkl_real=.true.
                 else
                   if(cond%transf_ind) then
                      h1=matmul(cond%transhkl,h1)
                   else
                      write(unit=*,fmt="(a,3f8.3)")" => Removed reflection (non-integer): ",h1
                      nr= nr-1
                      cycle
                   end if
                 end if
               else
                 if(cond%transf_ind) then
                    h1=matmul(cond%transhkl,h1)
                 else
                    h1=real(nint(h1))
                 end if
               end if
               Ob(nr)%hr=h1
            end do

        Case(7)    ! SXD data for FullProf

            if(.not. cond%cell_given) then
              Err_CFML%Msg=" => UNIT CELL not GIVEN! Modify your input file."
              Err_CFML%Ierr=1
              return
            end if

            if(cond%prop) cond%hkl_real=.true.

            do
               read(unit=inp,fmt="(a)",iostat=ier) line
               if( ier /= 0) exit
               if(line(1:1) == "#" .or. line(1:1) == "!") cycle
               if(line(1:1) == "(") then
                cond%forma=trim(line)
                read(unit=inp,fmt="(a)",iostat=ier) line
                if(cond%prop) then
                    read(unit=inp,fmt="(a)",iostat=ier) line
                    read(unit=line,fmt=*,iostat=ier) nkv
                    do i=1,nkv
                       read(unit=inp,fmt="(a)",iostat=ier) line
                    end do
                end if
                exit
               end if
            end do

            do
               read(unit=inp,fmt="(a)",iostat=ier) line
               if( ier /= 0) exit
               if(line(1:1) == "#" .or. line(1:1) == "!")  cycle
               nr=nr+1
               if(cond%prop) then
                 read(unit=line,fmt=cond%forma,iostat=ier) hkl(:), ivk,Ob(nr)%intens, Ob(nr)%sigma, Ob(nr)%icod, Ob(nr)%Lambda_Laue, &
                                                      Ob(nr)%twtheta,Ob(nr)%absorpt,Ob(nr)%tbar
                  if(ier /= 0 .or. (hkl(1)==0 .and. hkl(2)==0 .and. hkl(3) == 0 .and. ivk == 0) ) then
                   nr=nr-1
                   exit
                  end if
               else
                 read(unit=line,fmt=cond%forma,iostat=ier) hkl(:), Ob(nr)%intens, Ob(nr)%sigma, Ob(nr)%icod, Ob(nr)%Lambda_Laue, &
                                                      Ob(nr)%twtheta,Ob(nr)%absorpt,Ob(nr)%tbar
                  if(ier /= 0 .or. (hkl(1)==0 .and. hkl(2)==0 .and. hkl(3) == 0) ) then
                   nr=nr-1
                   exit
                  end if
               end if
               h1(:)=real(hkl(:))
               if(cond%transf_ind) then
                 h2=matmul(cond%transhkl,h1)
                 if(cond%prop) then
                   h3=matmul(cond%transhkl,kinfo%kv(:,ivk))
                   h2=h2+h3
                 end if
               else
                 h2=h1
                 if(cond%prop) h2=h2+kinfo%kv(:,ivk)
               end if
               Ob(nr)%hr=h2
            end do

        Case(8)       ! Flipping ratios from D3

            if(.not. cond%cell_given) then
              Err_CFML%Msg=" => UNIT CELL not GIVEN! Modify your input file."
              Err_CFML%Ierr=1
              return
            end if
            if(.not. cond%wave_given) then
              Err_CFML%Msg=" => WAVELENGTH not GIVEN! Modify your input file."
              Err_CFML%Ierr=1
              return
            end if

            cond%forma="(i8,6f8.3,2f10.2)"
            do
               nr=nr+1      ! gamma  omega  nu  => in fact it is: omega, gamma, nu!!!!
               read(unit=inp,fmt=cond%forma,iostat=ier) &
                   Ob(nr)%numor,h1, Ob(nr)%omega, Ob(nr)%phi,  Ob(nr)%chi,  Ob(nr)%intens, Ob(nr)%sigma
                   !                   ^omega     ^gamma    ^nu
               if(ier /= 0) then
                 nr=nr-1
                 exit
               end if
               if(.not. zbelong(h1)) then
                   if(cond%transf_ind) then
                      h1=matmul(cond%transhkl,h1)
                   else
                      write(unit=*,fmt="(a,3f8.3)")" => Removed reflection (non-integer): ",h1
                      nr= nr-1
                      cycle
                   end if
               else
                   if(cond%transf_ind) then
                      h1=matmul(cond%transhkl,h1)
                   else
                      h1=real(nint(h1))
                   end if
               end if
               Ob(nr)%hr=h1
            end do

        Case (9)  !i6,3f10.4,2f10.2,4f8.2  Suggested by Juerg Schefer
            cond%forma="(i6,3f10.4,2f10.2,4f8.2)"
            do
               nr=nr+1
               read(unit=inp,fmt="(i6,3f10.4,2f10.2,4f8.2)" ,iostat=ier) &
                    Ob(nr)%numor, h1, Ob(nr)%intens, Ob(nr)%sigma,Ob(nr)%twtheta, Ob(nr)%omega, Ob(nr)%chi, Ob(nr)%phi
               if(ier /= 0) then
                 nr=nr-1
                 exit
               end if

               if(.not. zbelong(h1)) then
                 if(cond%prop .or. cond%to_og) then
                   cond%hkl_real=.true.
                 else
                   if(cond%transf_ind) then
                      h1=matmul(cond%transhkl,h1)
                   else
                      write(unit=*,fmt="(a,3f8.3)")" => Removed reflection (non-integer): ",h1
                      nr= nr-1
                      cycle
                   end if
                 end if
               else
                 if(cond%transf_ind) then
                    h1=matmul(cond%transhkl,h1)
                 else
                    h1=real(nint(h1))
                 end if
               end if
               Ob(nr)%hr=h1
            end do


      End Select

      do i=1,nr
         h1=Ob(i)%hr
         if(cond%prop .or. cond%to_og) then
           Ob(i)%s=h_s(Ob(i)%hr,cell)
         else
           Ob(i)%h=nint(h1)
           Ob(i)%s=h_s(Ob(i)%h,cell)
         end if
         if(cond%hkl_type == 7) then
           Ob(i)%twtheta= 2.0* asind(Ob(i)%s*Ob(i)%Lambda_Laue)
         else
           Ob(i)%twtheta= 2.0* asind(Ob(i)%s*cond%wavel)
         end if
         if(Ob(i)%twtheta < 0.0001)  ier=1
         if(Ob(i)%intens  < 0.0001)  Ob(i)%intens=0.0001
         if(Ob(i)%sigma   < 0.00001) Ob(i)%sigma=sqrt(abs(Ob(i)%intens))
         if(cond%scale_given) then
           Ob(i)%intens= Ob(i)%intens * cond%scal_fact
           Ob(i)%sigma = Ob(i)%sigma  * cond%scal_fact
         end if
      end do

      Ref%nref=nr
      allocate(ind(nr))
      ind=sort(Ob(:)%s,nr)
      allocate(Ref%Ref(nr),source=Ob)


      found=.true.
      do i=1,nr
        j=ind(i)
        !Now calculate the integer indices
        if(cond%hkl_type /= 11 .and. cond%hkl_type /= 12) then
          if(cond%to_og) then !Transform the real indices to integers + og_kvect
             h1=Ob(j)%hr
             Ob(j)%h=nint(matmul(cond%transhkl,h1))
          else
             if(present(Gk)) then
               call get_integer_indices(Ob(j)%hr,kinfo,Ob(j)%h,found,Gk,Ob(j)%ekv)
             else
               call get_integer_indices(Ob(j)%hr,kinfo,Ob(j)%h,found)
             end if
          end if

          if(.not. found) then
             Ob(j)%pfn=1
             write(unit=*,fmt="(a,3f8.4)") " => Warning: no combination of the given Q_coefficients have been found for reflection: ",Ob(j)%hr
          end if
        end if
        Ref%Ref(i)=Ob(j)
      end do
    End Subroutine Read_Reflections_File

    Subroutine get_integer_indices(hr,kinfo,h,found,Gk,ekv)
      real(kind=cp), dimension(3),intent(in) :: hr
      type(kvect_Info_Type),      intent(in) :: kinfo
      integer, dimension(:),      intent(out):: h
      logical,                    intent(out):: found
      type(Group_k_Type),dimension(:),optional,intent(in)  :: Gk
      real(kind=cp),     dimension(3),optional,intent(out) :: ekv
      !
      integer                        :: i,j,k,L,m,nk,ia
      real(kind=cp)                  :: rv
      real(kind=cp), dimension(3)    :: ha,hc,kv
      integer,       dimension(3)    :: ih
      real(kind=cp), dimension(3,48,kinfo%nk) :: kvec

      ha=nint(hr)
      h=0; found=.false.;kvec=0.0
      if(sum(abs(ha-hr)) < epss) then  !Fundamental reflection
        h(1:3)=nint(hr)
        found=.true.
        return
      end if

      do i=1,3
        j=int(hr(i))
        rv=hr(i)-real(j,kind=cp)
        if(rv > 0.50001_cp) then
          ih(i)=nint(hr(i))
        else if(rv <= -0.50001_cp) then
          ih(i)=int(hr(i))
        else
          ih(i)=j
        end if
      end do

      ha=ih
      h(1:3)=ih
      nk=kinfo%nk

      if(present(Gk) .and. present(ekv)) then
        m=0
        do_j:do j=1,kinfo%nk
          do ia=-1,-1,-2
            do k=1,Gk(j)%ngk
              m=m+1
              kvec(:,m,j)=ia*Gk(j)%Eqv_k(:,k)
              !write(*,"(i8,3f10.4)") m, kvec(:,m,j)
            end do
          end do
        end do do_j

        do_i:do i=1,kinfo%nq
          do ia=1,-1,-2
            h(4:3+nk)=ia*kinfo%q_coeff(1:nk,i)
            do L=1,m
              kv=0.0
              do j=1,kinfo%nk
                  kv=kv+h(3+j)*kvec(:,L,j)
              end do
              hc=ha+kv
              if(sum(abs(hc-hr)) < epss) then !The good q_coeff are found!
                ekv=ia*kv
                found=.true.
                exit do_i
              end if
            end do
            !write(*,"(2i3,tr5,3f8.4,tr5,3f8.4,tr5,10i4)") i , ia, hr, hc, h
          end do !ia
        end do do_i

      else

        do_ii:do i=1,kinfo%nq
          do ia=1,-1,-2
              h(4:3+nk)=ia*kinfo%q_coeff(1:nk,i)
              kv=0.0
              do j=1,kinfo%nk
                kv=kv+h(3+j)*kinfo%kv(:,j)
              end do
              hc=ha+kv
              if(sum(abs(hc-hr)) < epss) then !The good q_coeff are found!
                found=.true.
                exit do_ii
              end if
          end do !ia
        end do do_ii

      end if

    End Subroutine get_integer_indices

    Subroutine Write_Reflections(R,cond,kinfo,lun,gen)
      type(Reflection_List),  intent(in) :: R
      type(Conditions_Type),  intent(in) :: cond
      type(kvect_Info_Type),  intent(in) :: kinfo
      integer, optional,      intent(in) :: lun
      logical, optional,      intent(in) :: gen

      integer :: iou,i,j,n,mr    !123456789012345678901234567890123456789
      character(len=41) :: fm ="(i7, i4,a,3f9.4,2f12.3,f14.6,3f8.4,2i8,a)"
      character(len=41) :: fm0 ="(i7, i4,a,3f9.4,2f12.3,f14.6,a)"
      character(len=:),allocatable :: line
      real(kind=cp), dimension(3)  :: hr

      iou=6
      if(present(lun)) iou=lun
      Select Case(kinfo%nk)
        case(0)
          if(present(gen)) then
             line="List of generated reflections"
          else
             line="List of read reflections"
          end if
        case default
          if(cond%hkl_type == 11 .or. cond%hkl_type == 12) then
             line="List of read reflections with real reciprocal space indices"
             if(present(gen)) line="List of generated reflections with real reciprocal space indices"
          else
             line="List of read reflections with extended integer indices"
          end if
      End Select
      write(unit=iou,fmt="(/,t5,a)") line
      line=repeat("=",len_trim(line))
      write(unit=iou,fmt="(t5,a)") line

      n=size(R%Ref(1)%h)
      write(unit=fm(5:5),fmt="(i1)") n
      write(unit=fm0(5:5),fmt="(i1)") n
      mr=0
      if(cond%hkl_type >= 11 .or. present(gen)) then
        Select Case(kinfo%nk)
          case(0)         !123456789012345678901234567890123456789
            line=" NumRef   h   k   l       Hr       Kr       Lr       Intens        Sigma    SinTL(1/2d)"
          case(1)
            line=" NumRef   h   k   l   m       Hr       Kr       Lr       Intens        Sigma    SinTL(1/2d)"
          case(2)
            line=" NumRef   h   k   l   m   n       Hr       Kr       Lr       Intens        Sigma    SinTL(1/2d)"
          case(3)
            line=" NumRef   h   k   l   m   n   p       Hr       Kr       Lr       Intens        Sigma    SinTL(1/2d)"
        End Select
        write(unit=iou,fmt="(a)") line
        if(present(gen)) then
           do i=1,R%nref
             write(unit=iou,fmt=fm0) i,R%Ref(i)%h,"  ",R%Ref(i)%hr,R%Ref(i)%intens,R%Ref(i)%sigma,R%Ref(i)%s,"   "//trim(inf(R%Ref(i)%imag))
           end do
        else
           do i=1,R%nref
             write(unit=iou,fmt=fm) i,R%Ref(i)%h,"  ",R%Ref(i)%hr,R%Ref(i)%intens,R%Ref(i)%sigma,R%Ref(i)%s
           end do
        end if
      else
        Select Case(kinfo%nk)
          case(0)         !123456789012345678901234567890123456789
            line=" NumRef   h   k   l       Hr       Kr       Lr       Intens        Sigma    SinTL(1/2d)       Character"
          case(1)
            line=" NumRef   h   k   l   m       Hr       Kr       Lr       Intens        Sigma    SinTL(1/2d)    Equivalent-Kv       Character"
          case(2)
            line=" NumRef   h   k   l   m   n       Hr       Kr       Lr       Intens        Sigma    SinTL(1/2d)    Equivalent-Kv       Character"
          case(3)
            line=" NumRef   h   k   l   m   n   p       Hr       Kr       Lr       Intens        Sigma    SinTL(1/2d)    Equivalent-Kv       Character"
        End Select
        write(unit=iou,fmt="(a)") line
        do i=1,R%nref
          hr=R%Ref(i)%h(1:3)
          do j=1,kinfo%nk
            hr=hr+R%Ref(i)%h(3+j)*kinfo%kv(:,j)
          end do
          if(R%Ref(i)%pfn == 1) then
            mr=mr+1
            write(unit=iou,fmt=fm) i,R%Ref(i)%h,"  ",R%Ref(i)%hr,R%Ref(i)%intens,R%Ref(i)%sigma,R%Ref(i)%s,hr,R%Ref(i)%pfn,R%Ref(i)%numor
          else
            if(kinfo%nk == 0) then
              write(unit=iou,fmt=fm) i,R%Ref(i)%h,"  ",R%Ref(i)%hr,R%Ref(i)%intens,R%Ref(i)%sigma,R%Ref(i)%s
            else
              write(unit=iou,fmt=fm) i,R%Ref(i)%h,"  ",R%Ref(i)%hr,R%Ref(i)%intens,R%Ref(i)%sigma,R%Ref(i)%s,R%Ref(i)%ekv
            end if
          end if
        end do
      end if
    End Subroutine Write_Reflections

  End Module DataRed_rnw_Reflections_Mod
