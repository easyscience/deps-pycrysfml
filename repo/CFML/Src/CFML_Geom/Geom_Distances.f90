 Submodule (CFML_Geom) Geom_Distances
    implicit none
  contains
    !!----
    !!---- Pure Module Function Distance(X0,X1,Cell or Code) Result(D)
    !!----    real(kind=cp), dimension(3),        intent(in) :: x0     !  In -> Point 1
    !!----    real(kind=cp), dimension(3),        intent(in) :: x1     !  In -> Point 2
    !!----    Type (Cell_G_Type),           intent(in) :: Cell   !  In -> Cell parameters
    !!----    Or
    !!----    real(kind=dp), dimension(3),        intent(in) :: x0     !  In -> Point 1
    !!----    real(kind=dp), dimension(3),        intent(in) :: x1     !  In -> Point 2
    !!----    Type (Cell_G_Type),           intent(in) :: Cell   !  In -> Cell parameters
    !!----    Or
    !!----    character(len=*), optional,         intent(in) :: Code
    !!----    real(kind=cp)                                  :: d      ! Out -> Distance
    !!----
    !!----    Calculate distance between two points.
    !!----       Fractional Coordinates: Use Cell
    !!----       Cartesian Coordiantes: Code="C" or Code=" "
    !!----       Spherical Coordinates: Code="S"
    !!----
    !!---- Update: February - 2005
    !!

    !!--++
    !!--++ Pure Module Function Distance_Fr(X0,X1,Celda) Result(D)
    !!--++    real(kind=cp), dimension(3),  intent(in) :: x0     !  In -> Point 1
    !!--++    real(kind=cp), dimension(3),  intent(in) :: x1     !  In -> Point 2
    !!--++    Type (Cell_G_Type),     intent(in) :: Celda  !  In -> Cell parameters
    !!--++    real(kind=cp)                                  :: d      ! Put -> Distance
    !!--++
    !!--++    (OVERLOADED)
    !!--++    Calculate distance between two points in Fractional
    !!--++
    !!--++ Update: February - 2005
    !!
    Pure Module Function Distance_Fr(X0,X1,Celda) Result(Dis)
       !---- Arguments ----!
       real(kind=cp), dimension(3), intent(in) :: x0,x1
       type (Cell_G_Type),    intent(in) :: Celda
       real(kind=cp)                           :: dis

       !---- Local Variables ----!
       real(kind=cp), dimension(3) :: xr

       xr = matmul(celda%Cr_Orth_cel,x1-x0)
       dis=sqrt(dot_product(xr,xr))

    End Function Distance_Fr

    !!--++
    !!--++ Pure Module Function Distance_Fr_dp(X0,X1,Celda) Result(D)
    !!--++    real(kind=dp), dimension(3),  intent(in) :: x0     !  In -> Point 1
    !!--++    real(kind=dp), dimension(3),  intent(in) :: x1     !  In -> Point 2
    !!--++    Type (Cell_G_Type),     intent(in) :: Celda  !  In -> Cell parameters
    !!--++    real(kind=dp)                            :: d      ! Put -> Distance
    !!--++
    !!--++    (OVERLOADED)
    !!--++    Calculate distance between two points in Fractional
    !!--++
    !!--++ Update: February - 2015
    !!
    Pure Module Function Distance_Fr_dp(X0,X1,Celda) Result(Dis)
       !---- Arguments ----!
       real(kind=dp), dimension(3), intent(in) :: x0,x1
       type (Cell_G_Type),    intent(in) :: Celda
       real(kind=dp)                           :: dis

       !---- Local Variables ----!
       real(kind=dp), dimension(3) :: xr

       xr = matmul(celda%Cr_Orth_cel,x1-x0)
       dis=sqrt(dot_product(xr,xr))

    End Function Distance_Fr_dp

    !!--++
    !!--++ Pure Module Function Distance_SC(X0,X1,Code) Result(D)
    !!--++    real(kind=cp), dimension(3),        intent(in) :: x0     !  In -> Point 1
    !!--++    real(kind=cp), dimension(3),        intent(in) :: x1     !  In -> Point 2
    !!--++    character(len=*), optional,         intent(in) :: Code
    !!--++    real(kind=cp)                                  :: d      ! Put -> Distance
    !!--++
    !!--++    (OVERLOADED)
    !!--++    Calculate distance between two points in Cartesian or Spherical
    !!--++    If Code =="C" or Blank or not present then the coordinates are Cartesian.
    !!--++    If Code =="S" then the coordinates are spherical (R, Theta, Phi).
    !!--++
    !!--++ Update: February - 2005
    !!
    Pure Module Function Distance_SC(X0,X1,Code) Result(Dis)
       !---- Arguments ----!
       real(kind=cp), dimension(3), intent(in) :: x0,x1
       character(len=*), optional,  intent(in) :: Code
       real(kind=cp)                           :: dis

       !---- Local Variables ----!
       real(kind=cp), dimension(3) :: xr,xi,xj

       xr=0.0
       if (present(code)) then
          select case (code(1:1))
             case("S","s") ! Spherical
                xi(1)=x0(1)*cosd(x0(3))*sind(x0(2))  ! R * cos(Phi) * sin(Theta)
                xi(2)=x0(1)*sind(x0(3))*sind(x0(2))  ! R * sin(Phi) * sin(Theta)
                xi(3)=x0(1)*cosd(x0(2))              ! R * cos(Theta)

                xj(1)=x1(1)*cosd(x1(3))*sind(x1(2))  ! R * cos(Phi) * sin(Theta)
                xj(2)=x1(1)*sind(x1(3))*sind(x1(2))  ! R * sin(Phi) * sin(Theta)
                xj(3)=x1(1)*cosd(x1(2))              ! R * cos(Theta)

                xr=xi-xj
             case("C","c") ! Cartesian
                xr=x1-x0
          end select
       else
          !---- Cartesian ----!
          xr=x1-x0
       end if
       dis=sqrt(dot_product(xr,xr))

    End Function Distance_SC

    !!----
    !!---- Module Subroutine Calc_Dist_Angle(Dmax, Dangl, Cell, Spg, A, Lun)
    !!----    real(kind=cp),        intent(in)   :: dmax   !  In -> Max. Distance to calculate
    !!----    real(kind=cp),        intent(in)   :: dangl  !  In -> Max. distance for angle calculations
    !!----    type (Cell_G_Type),   intent(in)   :: Cell   !  In -> Object of Crytal_Cell_Type
    !!----    Class(SpG_Type),      intent(in)   :: SpG    !  In -> Object of SpG_Type
    !!----    type (AtList_Type),   intent(in)   :: A      !  In -> Object of AtList_Type
    !!----    integer,  optional,   intent(in)   :: lun    !  In -> Logical Unit for writing
    !!----
    !!----    Subroutine to calculate distances and angles, below the prescribed distances
    !!----    "dmax" and "dangl" (angles of triplets at distance below "dangl" to an atom),
    !!----    without standard deviations. If dangl=0.0, no angle calculations are done.
    !!----    Needs as input the objects Cell (of type Crystal_cell), SpG (of type Space_Group)
    !!----    and A (of type atom_list, that should be allocated in the calling program).
    !!----    Writes results in file (unit=lun) if lun is present
    !!----    Control for error is present.
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine Calc_Dist_Angle(Dmax, Dangl, Cell, Spg, A, Lun)
       !---- Arguments ----!
       real(kind=cp),        intent(in)   :: Dmax, Dangl
       type (Cell_G_Type),   intent(in)   :: Cell
       Class(SpG_Type),      intent(in)   :: SpG
       type (AtList_Type),   intent(in)   :: A
       integer, optional,    intent(in)   :: lun

       !---- Local Variables ----!
       logical                            :: iprin
       integer                            :: i,j,k,lk,i1,i2,i3,jl,npeq,nn,L,nlines, max_coor,ico
       character(len= 80), dimension(12)  :: texto = " "
       character(len=  5)                 :: nam,nam1,nam2
       character(len= 40)                 :: transla
       character(len=160)                 :: form3
       character(len= 90)                 :: form2= "(a,3I4,a,a,a,a,a,f9.4,a,3F8.4,a,t85,a)"  !  JRC feb 2014 &   ! TR 4 fev. 2013
                                           !  "("" "",3I4,""  ("",a,"")-("",a,""):"",f9.4,""   "",3F8.4,""  "",a,""  "",a)"
       integer,          dimension(3)     :: ic1,ic2
       real(kind=cp),    dimension(3)     :: xx,x1,xo,Tn,xr, QD
       real(kind=cp)                      :: T,dd, da1,da2,da12,cang12,ang12,cang1,ang2,ang1

       real(kind=cp), allocatable,dimension(:,:) :: uu
       real(kind=cp), dimension(3,0:Spg%Num_lat) :: ltr
       real(kind=cp), allocatable,dimension(:,:) :: bcoo
       !real(kind=cp), dimension(3,SpG%Multip)    :: trr
       !integer,       dimension(3,3,SpG%Multip)  :: Mat

       !d=SpG%d
       !do i=1,SpG%Multip
       !  Mat(:,:,i)= SpG%Op(i)%Mat(1:3,1:3)
       !  trr(:,i)  = SpG%Op(i)%Mat(1:3,d)
       !End do
       if(.not. init_symOP) call init_opMatTr(SpG)

       ltr=0.0
       if(Spg%Num_lat > 0) then
          ltr(:,1:Spg%Num_lat)=Spg%Lat_tr(1:3,1:Spg%Num_lat)
       end if
       iprin=.false.
       if (present(lun)) then
          if (lun > 0) iprin=.true.
       end if

       call clear_error()

       call allocate_coordination_type(A%natoms,Spg%multip,Dmax,Max_coor)
       !write(*,"(a,3i6,f12.4)") " Max_coor,A%natoms,Spg%multip ",Max_coor, A%natoms,Spg%multip,dmax
       if(allocated(uu)) deallocate(uu)
       allocate(uu(3,Max_coor))
       if(allocated(bcoo)) deallocate(bcoo)
       allocate(bcoo(3,Max_coor))

       qd(:)=1.0/cell%rcell(:)
       ic2(:)= nint(dmax/cell%cell(:)+2.5_cp)
       ic1(:)=-ic2(:)
       npeq=spg%numops
       if (dangl > epsi .and. iprin ) then
          form3="(""    ("",a,"")-("",a,"")-("",a,""):"",f8.3/"
          form3=trim(form3)//"""    ("",a,"")-("",a,"")-("",a,""):"",f8.3/"
          form3=trim(form3)//"""    ("",a,"")-("",a,"")-("",a,""):"",f8.3/"
          form3=trim(form3)//"""         ("",a,"") :"",3f8.4,""  ("",a,"") :"",3f8.4)"
       end if

       if (spg%centred == 2) then
          npeq=2*npeq
          if (iprin) then
             write(unit=lun,fmt="(/,a)")" => Symmetry operators combined with inversion centre:"
             nlines=1
             do i=SpG%NumOps+1,npeq
                if (mod(i,2) == 0) then
                   write(unit=texto(nlines)(36:70),fmt="(a,i2,a,a)") &
                               " => SYMM(",i,"): ",trim(SpG%Symb_Op(i))
                   nlines=nlines+1
                else
                   write(unit=texto(nlines)( 1:34),fmt="(a,i2,a,a)")  &
                               " => SYMM(",i,"): ",trim(SpG%Symb_Op(i))
                end if
             end do
             do i=1,min(nlines,12)
                write(unit=lun,fmt="(a)") texto(i)
             end do
          end if
       end if

       do i=1,a%natoms
          xo(:)=a%atom(i)%x(:)
          nam=a%atom(i)%lab
          if (iprin) then
             write(unit=lun,fmt="(/,/,a)")"    -------------------------------------------------------------------"
             write(unit=lun,fmt="(a,f8.4,a,a,3f8.4)")   &
                       "    Distances less than",dmax,"  to atom: ",nam, xo
             write(unit=lun,fmt="(a,/,/)")"    -------------------------------------------------------------------"
             write(unit=lun,fmt="(/,/,a,/,/)") & ! TR 4 fev. 2013
             " Orig. extr. p.equiv.           Distance      x_ext   y_ext   z_ext  (tx,ty,tz)     Sym. op."
          end if
          Coord_Info%Coord_Num(i)=0
          ico=0
          do k=1,a%natoms
             lk=1
             uu(:,lk)=xo(:)
             nam1=a%atom(k)%lab
             do j=1,npeq
                xx=Matmul(opMat(:,:,j),a%atom(k)%x)+opTr(:,j)
                do i1=ic1(1),ic2(1)
                   do i2=ic1(2),ic2(2)
                      do i3=ic1(3),ic2(3)
                         do_jl:do jl=0,Spg%Num_Lat
                            Tn(:)=real([i1,i2,i3])+Ltr(:,jl)
                            x1(:)=xx(:)+tn(:)
                            do l=1,3
                               t=abs(x1(l)-xo(l))*qd(l)
                               if (t > dmax) cycle do_jl
                            end do
                            do nn=1,lk
                               if (sum(abs(uu(:,nn)-x1(:)))  <= epsi) cycle do_jl
                            end do
                            xr = matmul(cell%cr_orth_cel,x1-xo)
                            dd=sqrt(dot_product(xr,xr))
                            if (dd > dmax .or. dd < 0.001) cycle
                            ico=ico+1

                            if (Coord_Info%Coord_Num(i) > Coord_Info%Max_Coor) then
                               Err_CFML%Ierr=1
                               Err_CFML%flag=.true.
                               Err_CFML%Msg=" => Too many distances around atom: "//nam
                               return
                            end if

                            lk=lk+1
                            uu(:,lk)=x1(:)
                            Coord_Info%Dist(ico,i)=dd
                            Coord_Info%N_Cooatm(ico,i)=k
                            bcoo(:,ico)=x1(:)
                            Coord_Info%Tr_Coo(:,ico,i)=tn
                            if (iprin) then
                               transla= Frac_Trans_1Dig(tn)
                               write(unit=lun,fmt=form2) " ",i,k,j,"  (",nam,")-(",nam1,"):",dd,"   ",x1(:), "  "//transla, &
                                                         trim(SpG%Symb_Op(j)) !JRC Feb2014
                            end if
                         end do do_jl
                      end do !i3
                   end do !i2
                end do !i1
             end do !j
          end do !k

          Coord_Info%Coord_Num(i)=ico
          if (dangl <= epsi) cycle     !loop on "i" still running

          !---- Angle calculations for bonded atoms at distance lower than DANGL

          if (iprin) then
                write(unit=lun,fmt="(/,/,a)")       "   -------------------------------------------------------"
                write(unit=lun,fmt="(a,a,3f8.4)")   "   -  Angles around atom: ",nam, xo
                write(unit=lun,fmt="(a,/)")         "   -------------------------------------------------------"
          end if
          do j=1,Coord_Info%Coord_Num(i)
             if (Coord_Info%dist(j,i) < epsi .or. Coord_Info%dist(j,i) > dangl) cycle
             da1=Coord_Info%dist(j,i)
             i1=Coord_Info%N_Cooatm(j,i)
             nam1=a%atom(i1)%lab
             do k=j+1,Coord_Info%Coord_Num(i)
                if (Coord_Info%dist(k,i) < epsi .OR. Coord_Info%dist(k,i) > dangl) cycle
                da2=Coord_Info%dist(k,i)
                i2=Coord_Info%N_Cooatm(k,i)
                nam2=a%atom(i2)%lab
                xx(:)=bcoo(:,k)-bcoo(:,j)
                xr = matmul(Cell%Cr_Orth_cel,xx)
                da12=sqrt(dot_product(xr,xr))
                cang12=0.5_cp*(da1/da2+da2/da1-da12*da12/da1/da2)
                ang12=acosd(cang12)
                cang1=0.5_cp*(da12/da2+da2/da12-da1*da1/da12/da2)
                ang1=acosd(cang1)
                ang2=180.0_cp-ang1-ang12

                if (iprin) then
                    write(unit=lun,fmt="(/,3(a,f8.4))")  &
                         "     Atm-1   Atm-2   Atm-3            d12 =",da1,"  d23 =",da2,"   d13 =",da12
                    write(unit=lun,fmt=form3)  nam1,nam,nam2,ang12,   &
                         nam,nam2,nam1,ang1, nam,nam1,nam2,ang2,  &
                         nam1,bcoo(:,j),nam2, bcoo(:,k)
                end if
             end do !k
          end do !j
       end do !i

    End Subroutine Calc_Dist_Angle

    !!----
    !!---- Module Subroutine Calc_Dist_Angle_Sigma(Dmax, Dangl, Cell, Spg, A, Lun, Lun_cons, Lun_cif,filen,rdmax,ramin)
    !!----    real(kind=cp),             intent(in)   :: dmax     !  In -> Max. Distance to calculate
    !!----    real(kind=cp),             intent(in)   :: dangl    !  In -> Max. distance for angle calculations
    !!----    type (Cell_G_Type),        intent(in)   :: Cell     !  In -> Object of Crytal_Cell_Type
    !!----    Class(SpG_Type),           intent(in)   :: SpG      !  In -> Object of SpG_Type
    !!----    type (AtList_Type),        intent(in)   :: A        !  In -> Object of AtList_Type
    !!----    integer, optional,         intent(in)   :: lun      !  In -> Logical Unit for writing
    !!----    integer, optional,         intent(in)   :: lun_cons !  In -> Logical unit for writing restraints
    !!----    integer, optional,         intent(in)   :: lun_cif  !  In -> Logical unit for writing CIF file with distances and angles
    !!----    character(len=*), optional,intent(in)   :: filrest  !  In -> Name of file for writing restraints
    !!----    real(kind=cp),    optional,intent(in)   :: rdmax,ramin  !  Maximum distan and minimum angle for output in restraints file
    !!----
    !!----    Subroutine to calculate distances and angles, below the prescribed distances
    !!----    "dmax" and "dangl" (angles of triplets at distance below "dangl" to an atom),
    !!----    with standard deviations. If dangl=0.0, no angle calculations are done.
    !!----    Needs as input the objects Cell (of type Crystal_cell), SpG (of type Space_Group)
    !!----    and A (or type atom_list, that should be allocated in the calling program).
    !!----    Writes results in file (unit=lun) if the argument lun is present. In case
    !!----    lun_cif is provided, the program writes in the already opened CIF file (in
    !!----    the calling program) the items related to distances. If lun_cons is provided
    !!----    the program writes items containing restraints to the file CFML_restraints.tpcr
    !!----    or to file "filrest" if provided as argument.
    !!----    Control for error is present.
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine Calc_Dist_Angle_Sigma(Dmax, Dangl, Cell, Spg, A, Lun, Lun_cons, Lun_cif,filrest,rdmax,ramin)
       !---- Arguments ----!
       real(kind=cp),             intent(in)   :: dmax, dangl
       type (Cell_G_Type),        intent(in)   :: Cell
       Class(SpG_Type),           intent(in)   :: SpG
       type (AtList_Type),        intent(in)   :: A
       integer, optional,         intent(in)   :: lun
       integer, optional,         intent(in)   :: lun_cons
       integer, optional,         intent(in)   :: lun_cif
       character(len=*), optional,intent(in)   :: filrest
       real(kind=cp),    optional,intent(in)   :: rdmax, ramin

       !---- Local Variables ----!
       logical                            :: iprin
       integer,parameter                  :: nconst=3500
       integer                            :: i,j,k,lk,i1,i2,i3,jl,nn,L,ip,&
                                             itnum1,itnum2,num_const, max_coor,num_angc,ico
       character(len=  6)                 :: nam,nam1,nam2
       character(len= 40)                 :: transla
       character(len= 20)                 :: text,tex,texton
       character(len=132)                 :: line
       character(len=160)                 :: form3
       character(len= 90)                 :: form2= "(a,3i4,a,a,a,a,a,a12,3F8.4,a,t85,a)"  !  JRC feb 2014 form2= &   ! TR 4 fev. 2013
                                             !"("" "",3I4,""  ("",a,"")-("",a,""):"",f9.4,""   "",3F8.4,""  "",a,""  "",a)"
       integer, dimension(3)              :: ic1,ic2
       integer, dimension(3,3)            :: Rot
       integer, dimension(192)            :: itnum
       real(kind=cp),dimension(3,3,6)     :: DerM
       real(kind=cp),    dimension(3)     :: xx,x1,xo,Tn, QD,so,ss,s1,s2,x2,tr1,tr2
       real(kind=cp)                      :: T,dd, da1,da2,da12,cang12,ang12,cang1,ang2,ang1,rest_d,rest_a
       real(kind=cp)                      :: sdd,sda1,sda2,sda12,sang12,sang2,sang1,srel1,srel2,srel12

       real(kind=cp), allocatable, dimension(:,:) :: uu
       real(kind=cp), allocatable, dimension(:,:) :: bcoo
       real(kind=cp), allocatable, dimension(:,:) :: sbcoo
       real(kind=cp), allocatable, dimension(:,:) :: trcoo
       real(kind=cp), dimension(3,A%Natoms)       :: x_std

       character(len=132), dimension(:), allocatable  :: const_text
       character(len=132), dimension(:), allocatable  :: dist_text
       character(len=132), dimension(:), allocatable  :: angl_text

       character(len=8) :: codesym
       logical :: esta

       !--- write CIF ---------------------------------------------------------------------
       integer, parameter                             :: max_cif_dist_text = 1500
       integer, parameter                             :: max_cif_angl_text = 6000
       integer                                        :: n_cif_dist_text
       integer                                        :: n_cif_angl_text
       character (len=12)                             :: CIF_bond_site_symm_2
       character (len=12)                             :: CIF_angle_site_symm_1
       character (len=12)                             :: CIF_angle_site_symm_3
       character (len=132), dimension(:), allocatable :: cif_dist_text
       character (len=132), dimension(:), allocatable :: cif_angl_text
       !-----------------------------------------------------------------------------------
       !real(kind=cp), dimension(3,SpG%Multip)    :: trr
       !integer,       dimension(3,3,SpG%Multip)  :: Mat
       !
       !d=SpG%d
       !do i=1,SpG%Multip
       !  Mat(:,:,i)= SpG%Op(i)%Mat(1:3,1:3)
       !  trr(:,i)  = SpG%Op(i)%Mat(1:3,d)
       !End do
       if(.not. init_symOP) call init_opMatTr(SpG)
       iprin=.false.
       if (present(lun)) then
          if (lun > 0) iprin=.true.
       end if
       rest_d=dmax
       rest_a=45.0
       if(present(rdmax)) rest_d=rdmax
       if(present(ramin)) rest_a=ramin
       call clear_error()
       call Allocate_Coordination_Type(A%natoms,Spg%Multip,Dmax,max_coor)

       if(allocated(uu)) deallocate(uu)
       allocate(uu(3,max_coor))
       if(allocated(bcoo)) deallocate(bcoo)
       allocate(bcoo(3,max_coor))
       if(allocated(sbcoo)) deallocate(sbcoo)
       allocate(sbcoo(3,max_coor))
       if(allocated(trcoo)) deallocate(trcoo)
       allocate(trcoo(3,max_coor))
       x_std=0.0
       Select Type(ats => A%Atom)
         Type is(Atm_Std_Type)
          do i=1,A%Natoms
            x_std(:,i)=Ats(i)%x_std(:)
          end do
       End Select
       DerM= get_deriv_Orth_cell(cell,"A ")

       if (present(lun_cons)) then
          num_angc=0
          num_const=0
          if(present(filrest)) then
            open (unit=lun_cons, file=trim(filrest), status="replace", action="write")
          else
            open (unit=lun_cons, file="CFML_Restraints.tpcr", status="replace", action="write")
          end if
          write(unit=lun_cons,fmt="(a)") " FILE with lines for soft distance and angle constraints (restraints)."
          write(unit=lun_cons,fmt="(a)") " It is intended to help editing PCR files with restraints by pasting, "
          write(unit=lun_cons,fmt="(a)") " after correcting the values as wished, to the appropriate lines.  "
          write(unit=lun_cons,fmt="(a)") " Lines with repeated identical distances have been excluded because symmetry "
          write(unit=lun_cons,fmt="(a)") " already force a hard constraint."
          write(unit=lun_cons,fmt="(a)") " Accidental coincidences have also been excluded, check that in list of distances! "
          write(unit=lun_cons,fmt="(/,a)")   " Warning! "
          write(unit=lun_cons,fmt="(a,/,a/)") " Symmetry constrained angles have not been eliminated,",&
                                              " this has to be performed by hand!"

          !---- Set ITnum ----!
          i=0
          i1=1
          i2=36
          do j=1,Spg%multip
             rot=OpMat(:,:,j)
             Itnum(j)=searchop(rot,i1,i2)
          end do
          if (allocated(const_text)) deallocate(const_text)
          allocate(const_text(nconst)) !Maximum number of restraints
          const_text(:)(1:132)=" "
          if (allocated(dist_text)) deallocate(dist_text)
          allocate(dist_text(nconst)) !Maximum number of restraints
          dist_text(:)(1:132)=" "
          if (allocated(angl_text)) deallocate(angl_text)
          allocate(angl_text(nconst)) !Maximum number of restraints
          angl_text(:)(1:132)=" "
       end if

       if (present(lun_cif)) then
          write(unit=lun_cif, fmt='(a)') " "
          write(unit=lun_cif, fmt='(a)') "#=============================================================================#"
          write(unit=lun_cif, fmt='(a)') "#                      UNIT CELL INFORMATION                                  #"
          write(unit=lun_cif, fmt='(a)') "#=============================================================================#"
          write(unit=lun_cif, fmt='(a)') "_symmetry_cell_setting                "//trim(SPG%CrystalSys)
          write(unit=lun_cif, fmt='(a)') "_symmetry_space_group_name_H-M       '"//trim(SPG%SPG_symb)//"'"
          write(unit=lun_cif, fmt='(a)') "_symmetry_space_group_name_Hall      '"//trim(SPG%Hall)//"'"
          write(unit=lun_cif, fmt='(a)') " "
          write(unit=lun_cif, fmt='(a)') "loop_"
          write(unit=lun_cif, fmt='(a)') "    _symmetry_equiv_pos_as_xyz   #<--must include 'x,y,z'"

          do i=1,SPG%multip
             write(unit=lun_cif, fmt='(a)') "'"//trim(SpG%Symb_Op(i))//"'"
          end do
          write(unit=lun_cif, fmt='(a)') " "

          write(unit=lun_cif, fmt='(a)') "#=============================================================================#"
          write(unit=lun_cif, fmt='(a)') "#                       MOLECULAR GEOMETRY                                    #"
          write(unit=lun_cif, fmt='(a)') "#=============================================================================#"

          if (allocated(CIF_dist_text)) deallocate(CIF_dist_text)
          allocate(CIF_dist_text(max_cif_dist_text)) !Maximum number of distances
          CIF_dist_text(:)(1:132)=" "
          if (allocated(CIF_angl_text)) deallocate(CIF_angl_text)
          allocate(CIF_angl_text(max_cif_angl_text)) !Maximum number of angles
          CIF_angl_text(:)(1:132)=" "
          n_cif_dist_text = 0
          n_cif_angl_text = 0
       end if

       qd(:)=1.0/cell%rcell(:)
       ic2(:)= nint(dmax/cell%cell(:)+2.5_cp)
       ic1(:)=-ic2(:)
       if (dangl > epsi .and. iprin ) then
          form3=            "(""    ("",a,"")-("",a,"")-("",a,""):"",a12/"
          form3=trim(form3)//"""    ("",a,"")-("",a,"")-("",a,""):"",a12/"
          form3=trim(form3)//"""    ("",a,"")-("",a,"")-("",a,""):"",a12/"
          form3=trim(form3)//"""         ("",a,"") :"",3f9.5,""  ("",a,"") :"",3f9.5)"
       end if
       do i=1,a%natoms
          xo(:)=a%atom(i)%x(:)
          so(:)=x_std(:,i)
          nam=a%atom(i)%lab
          Select Case (len_trim(nam))
             case(1)
                nam="  "//trim(nam)
             case(2:5)
                nam=" "//trim(nam)
          End Select
          if (iprin) then
             write(unit=lun,fmt="(/,/,a)")"    -------------------------------------------------------------------"
             write(unit=lun,fmt="(a,f8.4,a,a,3f8.4)")   &
                       "    Distances less than",dmax,"  to atom: ",nam, xo
             write(unit=lun,fmt="(a,/,/)")"    -------------------------------------------------------------------"
             write(unit=lun,fmt="(/,/,a,/,/)") &    ! TR 4 fev. 2013
                  " Orig. extr. p.equiv.           Distance      x_ext   y_ext   z_ext  (tx,ty,tz)     Sym. op."
          end if

          ico=0
          do k=1,a%natoms
             lk=1
             uu(:,lk)=xo(:)
             nam1=a%atom(k)%lab
             Select Case (len_trim(nam1))
               case(1)
                  nam1="  "//trim(nam1)
               case(2:5)
                  nam1=" "//trim(nam1)
             End Select
             ss(:)=x_std(:,k)
             do j=1,Spg%Multip
                xx=Matmul(opMat(:,:,j),a%atom(k)%x)+opTr(:,j)
                do i1=ic1(1),ic2(1)
                   do i2=ic1(2),ic2(2)
                      do_i3:do i3=ic1(3),ic2(3)
                            Tn(:)=real([i1,i2,i3])
                            x1(:)=xx(:)+tn(:)
                            do l=1,3
                               t=abs(x1(l)-xo(l))*qd(l)
                               if (t > dmax) cycle  do_i3
                            end do
                            do nn=1,lk
                               if (sum(abs(uu(:,nn)-x1(:)))  <= epsi) cycle  do_i3
                            end do
                            call distance_and_sigma(Cell,DerM,xo,x1,so,ss,dd,sdd)
                            if (dd > dmax .or. dd < 0.001) cycle
                            ico=ico+1
                            if (Coord_Info%Coord_Num(i) > Coord_Info%Max_Coor) then
                               Err_CFML%Ierr=1
                               Err_CFML%flag=.true.
                               Err_CFML%Msg=" => Too many distances around atom: "//nam
                               return
                            end if
                            lk=lk+1
                            uu(:,lk)=x1(:)

                            Coord_Info%Dist(ico,i)=dd
                            Coord_Info%S_Dist(ico,i)=sdd
                            Coord_Info%N_Cooatm(ico,i)=k
                            Coord_Info%N_sym(ico,i)=j
                            Coord_Info%Tr_Coo(:,ico,i)=tn

                            bcoo(:,ico)=x1(:)
                            sbcoo(:,ico)=ss(:)
                            trcoo(:,ico)=Tn(:)
                            if (iprin) then
                               transla= Frac_Trans_1Dig(tn)
                               text=String_NumStd(dd,sdd)
                               !write(unit=lun,fmt=form2) i,k,j,nam,nam1,dd,x1(:), transla, trim(SpG%Symb_Op(j))! TR 4 fev. 2013
                               write(unit=lun,fmt=form2) " ",i,k,j,"  (",nam,")-(",nam1,"):",text,x1(:), "  "//transla, &
                                                         trim(SpG%Symb_Op(j)) !JRC Feb2014
                            end if

                            if(present(lun_cons) .and. dd <= rest_d) then
                              esta=.false.
                              write(unit=line,fmt="(a4,tr2,a4,i5,3f10.5,tr5,2f7.4)") A%atom(i)%lab ,A%atom(k)%lab ,&
                                     Itnum(j), tn+opTr(:,j) ,dd, sdd
                              if(num_const == 0) then
                                const_text(1)=line(1:132)
                                num_const=1
                                write(unit=dist_text(1),fmt="(a,2f9.5,a)") "DFIX ",dd,sdd, &
                                                                           "  "//trim(A%atom(i)%lab)//"  "//trim(A%atom(k)%lab)
                                call Write_SymTrans_Code(j,tn,ChCode=codesym)
                                dist_text(1)=trim(dist_text(1))//codesym
                              else
                                do l=num_const,1,-1
                                 if( (line(1:4) == const_text(l)(1:4) .and. line(7:10) == const_text(l)(7:10)) .or. &
                                     (line(1:4) == const_text(l)(7:10) .and. line(7:10) == const_text(l)(1:4)) ) then
                                   if(line(51:132) == const_text(l)(51:132)) then
                                        esta=.true.
                                        exit
                                   end if
                                 end if
                                end do
                                if(.not. esta) then
                                  num_const=num_const+1
                                  if(num_const > NCONST) then
                                     num_const=num_const-1
                                  end if
                                  const_text(num_const)=line(1:132)
                                  write(unit=dist_text(num_const),fmt="(a,2f9.5,a)") "DFIX ",dd,sdd,&
                                        "  "//trim(A%atom(i)%lab)//"  "//trim(A%atom(k)%lab)
                                  call Write_SymTrans_Code(j,tn,ChCode=codesym)
                                  dist_text(num_const)=trim(dist_text(num_const))//trim(codesym)
                                end if
                              end if
                            end if

                            if(present(lun_cif) .and. n_cif_dist_text < max_cif_dist_text) then
                               text=String_NumStd(dd,sdd)
                               n_cif_dist_text = n_cif_dist_text + 1

                               !if(i1==0 .and. i2==0 .and. i3==0 .and. j==1) then
                               ! write(unit=CIF_bond_site_symm_2, fmt='(a)') "       . ?"
                               !else
                                write(unit=CIF_bond_site_symm_2, fmt='(a,i3, a, 3i1,a)') " ", j, "_", nint(tn+5.0), " ?"
                               !end if

                               write(unit=CIF_dist_text(n_cif_dist_text), fmt='(6a)') &
                                     A%atom(i)%lab(1:4), "  ", A%atom(k)%lab(1:4), " ", text(1:12), CIF_bond_site_symm_2
                            end if
                      end do do_i3 !i3
                   end do !i2
                end do !i1
             end do !j
          end do !k

          Coord_Info%Coord_Num(i)=ico
          if (dangl <= epsi) cycle     !loop on "i" still running

          !---- Angle calculations for bonded atoms at distance lower than DANGL
          if (present(lun_cons)) write(unit=lun_cons,fmt="(a,a)")"=> Help for possible angle restraints around atom ",A%atom(i)%lab

          if (iprin) then
             write(unit=lun,fmt="(/,/,a)")       "   -------------------------------------------------------"
             write(unit=lun,fmt="(a,a,3f8.4)")   "   -  Angles around atom: ",nam, xo
             write(unit=lun,fmt="(a,/)")         "   -------------------------------------------------------"
          end if
          do j=1,Coord_Info%Coord_Num(i)
             if (Coord_Info%Dist(j,i) < epsi .or. Coord_Info%Dist(j,i) > dangl) cycle
             da1=Coord_Info%Dist(j,i)
             sda1=Coord_Info%S_Dist(j,i)
             i1=Coord_Info%N_Cooatm(j,i)
             nam1=a%atom(i1)%lab
             Select Case (len_trim(nam1))
               case(1)
                  nam1="  "//trim(nam1)
               case(2:5)
                  nam1=" "//trim(nam1)
             End Select
             if (present(lun_cons)) then
               ip=Coord_Info%N_sym(j,i)
               itnum1=itnum(ip)
               tr1(:)=trcoo(:,j)+ opTr(:,ip)  !real(Spg%Op(Coord_Info%N_sym(j,i))%Mat(1:3,4))
             end if
             do k=j+1,Coord_Info%Coord_Num(i)
                if (Coord_Info%Dist(k,i) < epsi .OR. Coord_Info%Dist(k,i) > dangl) cycle
                da2=Coord_Info%Dist(k,i)
                sda2=Coord_Info%S_Dist(k,i)
                i2=Coord_Info%N_Cooatm(k,i)
                nam2=a%atom(i2)%lab
                Select Case (len_trim(nam2))
                  case(1)
                     nam2="  "//trim(nam2)
                  case(2:5)
                     nam2=" "//trim(nam2)
                End Select
                if (present(lun_cons)) then
                  ip=Coord_Info%N_sym(k,i)
                  itnum2=itnum(ip)
                  tr2(:)=trcoo(:,k)+ opTr(:,ip) !real(Spg%Op(Coord_Info%N_sym(k,i))%Mat(1:3,4))
                  !tr2(:)=trcoo(:,k)+trr(:,Coord_Info%N_sym(k,i))
                end if
                x1(:)=bcoo(:,k)
                x2(:)=bcoo(:,j)
                s1(:)=sbcoo(:,k)
                s2(:)=sbcoo(:,j)
                call distance_and_sigma(Cell,derM,x1,x2,s1,s2,da12,sda12)
                if( da12 < 0.0001) cycle

                cang12=0.5_cp*(da1/da2+da2/da1-da12*da12/da1/da2)
                ang12=ACOSd(cang12)
                cang1=0.5_cp*(da12/da2+da2/da12-da1*da1/da12/da2)
                ang1=ACOSd(cang1)
                ang2=180.0_cp-ang1-ang12

               ! if(abs(abs(cang12)-1.0) < 0.0001) then
               !   sang12=0.0
               ! else
               !  dcang121=(1.0/da2-cang12/da1)**2
               !  dcang122=(1.0/da1-cang12/da2)**2
               !  dcang1212=(da12/da2/da1)**2
               !  sang12=sqrt((dcang121*sda1**2+dcang122*sda2**2+dcang1212*sda12**2)/(1.0-cang12**2))*to_deg
               ! end if
               ! if(abs(abs(cang1)-1.0) < 0.0001) then
               !   sang1=0.0
               ! else
               !  dcang112=(1.0/da2-cang1/da12)**2
               !  dcang12=(1.0/da12-cang1/da2)**2
               !  dcang11=(da1/da2/da12)**2
               !  sang1=sqrt((dcang11*sda1**2+dcang12*sda2**2+dcang112*sda12**2)/(1.0-cang1**2))*to_deg
               ! end if
               ! sang2=sqrt(sang1**2+sang12**2)


                !---- Alternative calculation of angles' sigmas ----!
                srel1=(sda1/da1)**2
                srel12=(sda12/da12)**2
                srel2=(sda2/da2)**2
                sang12=SQRT(srel1+srel2+(sda12*da12/da1/da2)**2)*to_deg
                sang1=SQRT(srel12+srel2+(sda1*da1/da2/da12)**2)*to_deg
                sang2=SQRT(srel12+srel1+(sda2*da2/da1/da12)**2)*to_deg

                if (iprin) then
                   tex=String_NumStd(da1,sda1)
                   text= String_NumStd(da2,sda2)
                   texton= String_NumStd(da12,sda12)
                   write(unit=lun,fmt="(/,a,3a21)")  &
                        "     Atm-1   Atm-2   Atm-3           "," d12 ="//tex,"  d23 ="//text,"   d13 ="//texton
                      tex=String_NumStd(ang12,sang12)
                     text=String_NumStd(ang1,sang1)
                   texton=String_NumStd(ang2,sang2)
                   write(unit=lun,fmt=form3)  nam1,nam,nam2,tex,    &
                                              nam,nam2,nam1,text,   &
                                              nam,nam1,nam2,texton, &
                                              nam1,bcoo(:,j),  nam2, bcoo(:,k)
                end if

                if (present(lun_cons)) then

                  if(ang2 >= rest_a) &
                  write(unit=lun_cons,fmt="(3(a6,tr1),i3,i4,tr1,3f8.4,tr1,3f8.4,2f7.2)") &
                  A%atom(i)%lab ,nam1 ,nam2 ,itnum1,itnum2,tr1(:),tr2(:),ang2,sang2

                  if(ang1 >= rest_a) &
                  write(unit=lun_cons,fmt="(3(a6,tr1),i3,i4,tr1,3f8.4,tr1,3f8.4,2f7.2)") &  !Another angle of the same triangle
                  A%atom(i)%lab ,nam2 ,nam1 ,itnum2,itnum1,tr2(:),tr1(:),ang1,sang1

                  if(ang12 >= rest_a .and. itnum1==1 .and. sum(abs(tr1)) < 0.001) & !Good constraint
                  write(unit=lun_cons,fmt="(3(a6,tr1),i3,i4,tr1,3f8.4,tr1,3f8.4,2f7.2)") &
                  adjustl(nam1),A%atom(i)%lab ,nam2 ,itnum1,itnum2,tr1(:),tr2(:),ang12,sang12

                  if(ang12 >= rest_a .and. itnum2==1 .and. sum(abs(tr2)) < 0.001) & !Good constraint
                  write(unit=lun_cons,fmt="(3(a6,tr1),i3,i4,tr1,3f8.4,tr1,3f8.4,2f7.2)") &  !Another angle of the same triangle
                  adjustl(nam2)," "//A%atom(i)%lab ,nam1 ,itnum2,itnum1,tr2(:),tr1(:),ang12,sang12

                  if(num_angc == 0) then

                    if(ang2 >= rest_a) then
                      num_angc=num_angc+1
                      line=" "
                      write(unit=line,fmt="(a,2f9.3,a)") "AFIX ",ang2,sang2,&
                                                         "  "//trim(A%atom(i)%lab)//" "//trim(nam1)
                      call Write_SymTrans_Code(Coord_Info%N_sym(j,i),trcoo(:,j),ChCode=codesym)
                      line=trim(line)//trim(codesym)//" "//trim(nam2)
                      call Write_SymTrans_Code(Coord_Info%N_sym(k,i),trcoo(:,k),ChCode=codesym)
                      line=trim(line)//trim(codesym)
                      angl_text(1)=line(1:132)
                    end if
                    !Repeating with another angle of the same triangle
                    if(ang1 >= rest_a) then
                      num_angc=num_angc+1
                      line=" "
                      write(unit=line,fmt="(a,2f9.3,a)") "AFIX ",ang1,sang1,&
                                                         "  "//trim(A%atom(i)%lab)//" "//trim(nam2)
                      call Write_SymTrans_Code(Coord_Info%N_sym(k,i),trcoo(:,k),ChCode=codesym)
                      line=trim(line)//trim(codesym)//" "//trim(nam1)
                      call Write_SymTrans_Code(Coord_Info%N_sym(j,i),trcoo(:,j),ChCode=codesym)
                      line=trim(line)//trim(codesym)
                      angl_text(num_angc)=line(1:132)
                    end if

                    if(ang12 >= rest_a .and. itnum1==1 .and. sum(abs(tr1)) < 0.001) then
                      num_angc=num_angc+1
                      line=" "
                      write(unit=line,fmt="(a,2f9.3,a)") "AFIX ",ang12,sang12,&
                                                         " "//trim(nam1)//"  "//trim(A%atom(i)%lab)
                      call Write_SymTrans_Code(1,[0.0,0.0,0.0],ChCode=codesym)
                      line=trim(line)//trim(codesym)//" "//trim(nam2)
                      call Write_SymTrans_Code(Coord_Info%N_sym(k,i),trcoo(:,k),ChCode=codesym)
                      line=trim(line)//trim(codesym)
                      angl_text(num_angc)=line(1:132)
                    end if

                    if(ang12 >= rest_a .and. itnum2==1 .and. sum(abs(tr2)) < 0.001) then
                      num_angc=num_angc+1
                      line=" "
                      write(unit=line,fmt="(a,2f9.3,a)") "AFIX ",ang12,sang12,&
                                                         " "//trim(nam2)//"  "//trim(A%atom(i)%lab)
                      call Write_SymTrans_Code(1,[0.0,0.0,0.0],ChCode=codesym)
                      line=trim(line)//trim(codesym)//" "//trim(nam1)
                      call Write_SymTrans_Code(Coord_Info%N_sym(j,i),trcoo(:,j),ChCode=codesym)
                      line=trim(line)//trim(codesym)
                      angl_text(num_angc)=line(1:132)
                    end if

                  else

                    if(ang2 >= rest_a) then
                      line=" "
                      write(unit=line,fmt="(a,2f9.3,a)") "AFIX ",ang2,sang2,&
                                                         "  "//trim(A%atom(i)%lab)//" "//trim(nam1)
                      call Write_SymTrans_Code(Coord_Info%N_sym(j,i),trcoo(:,j),ChCode=codesym)
                      line=trim(line)//trim(codesym)//" "//trim(nam2)
                      call Write_SymTrans_Code(Coord_Info%N_sym(k,i),trcoo(:,k),ChCode=codesym)
                      line=trim(line)//trim(codesym)

                      esta=.false.
                      jl=index(line,"_")
                      if(jl == 0) jl=len_trim(line)
                      do l=num_angc,1,-1
                       if( line(1:jl) == angl_text(l)(1:jl)) then
                           esta=.true.
                           exit
                       end if
                      end do
                      if(.not. esta) then
                        num_angc=num_angc+1
                        if(num_angc > NCONST) num_angc=NCONST
                        angl_text(num_angc)=line(1:132)
                      end if
                    end if

                    if(ang1 >= rest_a) then
                      !Repeating with another angle of the same triangle
                      line=" "
                      write(unit=line,fmt="(a,2f9.3,a)") "AFIX ",ang1,sang1,&
                                                         "  "//trim(A%atom(i)%lab)//" "//trim(nam2)
                      call Write_SymTrans_Code(Coord_Info%N_sym(k,i),trcoo(:,k),ChCode=codesym)
                      line=trim(line)//trim(codesym)//" "//trim(nam1)
                      call Write_SymTrans_Code(Coord_Info%N_sym(j,i),trcoo(:,j),ChCode=codesym)
                      line=trim(line)//trim(codesym)

                      esta=.false.
                      jl=index(line,"_")
                      if(jl == 0) jl=len_trim(line)
                      do l=num_angc,1,-1
                       if( line(1:jl) == angl_text(l)(1:jl)) then
                           esta=.true.
                           exit
                       end if
                      end do
                      if(.not. esta) then
                        num_angc=num_angc+1
                        if(num_angc > NCONST) num_angc=NCONST
                        angl_text(num_angc)=line(1:132)
                      end if
                    end if

                    if(ang12 >= rest_a .and. itnum1==1 .and. sum(abs(tr1)) < 0.001) then
                      !Repeating with another angle of the same triangle
                      line=" "
                      write(unit=line,fmt="(a,2f9.3,a)") "AFIX ",ang12,sang12,&
                                                          " "//trim(nam1)//"  "//trim(A%atom(i)%lab)
                      call Write_SymTrans_Code(1,[0.0,0.0,0.0],ChCode=codesym)
                      line=trim(line)//trim(codesym)//" "//trim(nam2)
                      call Write_SymTrans_Code(Coord_Info%N_sym(k,i),trcoo(:,k),ChCode=codesym)
                      line=trim(line)//trim(codesym)

                      esta=.false.
                      jl=index(line,"_")
                      if(jl == 0) jl=len_trim(line)
                      do l=num_angc,1,-1
                       if( line(1:jl) == angl_text(l)(1:jl)) then
                           esta=.true.
                           exit
                       end if
                      end do
                      if(.not. esta) then
                        num_angc=num_angc+1
                        if(num_angc > NCONST) num_angc=NCONST
                        angl_text(num_angc)=line(1:132)
                      end if
                    end if

                    if(ang12 >= rest_a .and. itnum1==2 .and. sum(abs(tr2)) < 0.001) then
                      !Repeating with another angle of the same triangle
                      line=" "
                      write(unit=line,fmt="(a,2f9.3,a)") "AFIX ",ang12,sang12,&
                                                          " "//trim(nam2)//"  "//trim(A%atom(i)%lab)
                      call Write_SymTrans_Code(1,[0.0,0.0,0.0],ChCode=codesym)
                      line=trim(line)//trim(codesym)//" "//trim(nam1)
                      call Write_SymTrans_Code(Coord_Info%N_sym(j,i),trcoo(:,j),ChCode=codesym)
                      line=trim(line)//trim(codesym)

                      esta=.false.
                      jl=index(line,"_")
                      if(jl == 0) jl=len_trim(line)
                      do l=num_angc,1,-1
                       if( line(1:jl) == angl_text(l)(1:jl)) then
                           esta=.true.
                           exit
                       end if
                      end do
                      if(.not. esta) then
                        num_angc=num_angc+1
                        if(num_angc > NCONST) num_angc=NCONST
                        angl_text(num_angc)=line(1:132)
                      end if
                    end if

                  end if

                end if !present(lun_cons)

                if (present(lun_cif) .and. n_cif_angl_text < max_cif_angl_text .and. ang12 > 45.0) then
                   !--- Change: I have included the condition ang12 > 45 for selecting the angle to write in
                   !            the CIF file, normally the angles below that value are irrelevant from the
                   !            chemical point of view.
                   ! j: indice de l'operateur de symetrie pour atome 1 ----! No!, Now j and k correspond to indices
                   ! k: indice de l'operateur de symetrie pour atome 2 ----!      running on coordination around atom i!
                   ! tr1: translation associee a op_j ----No!      =>  trcoo(:,j)!
                   ! tr2: translation associee a op_k ?  ----No!   =>  trcoo(:,k)!
                   n_cif_angl_text = n_cif_angl_text + 1

                   !  The commented lines correspond to wrong selections of translations!!!!!!!
                   !  Moreover the indices j and k were taken as the ordinal numbers of the
                   !  symmetry operators and that's not true!
                   !if (j==1 .and. nint(tr1(1))==0 .and. nint(tr1(2))==0 .and. nint(tr1(3))==0) then
                   !   write(unit=CIF_angle_site_symm_1, fmt='(a)') "       ."
                   !else
                   !   write(unit=CIF_angle_site_symm_1, fmt='(a,i3, a, 3I1)') " ", j, "_",  &
                   !         nint(tr1(1)+5.0), nint(tr1(2)+5.0), nint(tr1(3)+5.0)
                   !end if
                   !if (k==1 .and. nint(tr2(1))==0 .and. nint(tr2(2))==0 .and. nint(tr2(3))==0) then
                   !   write(unit=CIF_angle_site_symm_3, fmt='(a)') "  .  ?"
                   !else
                   !   write(unit=CIF_angle_site_symm_3, fmt='(a,i3, a, 3I1,a)') " ", k, "_", &
                   !         nint(tr2(1)+5.0), nint(tr2(2)+5.0), nint(tr2(3)+5.0), " ?"
                   !end if

                   write(unit=CIF_angle_site_symm_1, fmt='(a,i3, a, 3I1)') " ", &
                         Coord_Info%N_sym(j,i), "_", nint(trcoo(:,j)+5.0)
                   write(unit=CIF_angle_site_symm_3, fmt='(a,i3, a, 3I1,a)') " ", &
                         Coord_Info%N_sym(k,i), "_", nint(trcoo(:,k)+5.0), " ?"

                   write(unit=CIF_angl_text(n_cif_angl_text), fmt='(10a)')        &
                         nam1(1:4)," ", nam(1:4), " ",nam2, tex(1:12), " ",       &
                         trim(CIF_angle_site_symm_1), " ", trim(CIF_angle_site_symm_3)

                end if
             end do !k
          end do !j
       end do !i

       if (present(lun_cons)) then
          write(unit=lun_cons,fmt="(/,a,i5)")"=> Total number of independent distances: ",num_const
          write(unit=lun_cons,fmt="(a,/)")   "   List of possible restraints: "
          write(unit=lun_cons,fmt="(a)")" At1   At2  ITnum     T1        T2        T3          DIST   SIGMA"
          do i=1,num_const
             write(unit=lun_cons,fmt="(2x,a)") trim(const_text(i))
          end do

          write(unit=lun_cons,fmt="(/,a)")   "   ========================================= "
          write(unit=lun_cons,fmt="(a  )")   "   List of possible restraints in CFL format "
          write(unit=lun_cons,fmt="(a,/)")   "   ========================================= "


          write(unit=lun_cons,fmt="(/a,i5)")"=> Total number of independent distance restraints: ",num_const
          do i=1,num_const
             write(unit=lun_cons,fmt="(a)") trim(dist_text(i))
          end do
          write(unit=lun_cons,fmt="(/a,i5)")"=> Total number of possible angle restraints: ",num_angc
          do i=1,num_angc
             write(unit=lun_cons,fmt="(a)") trim(angl_text(i))
          end do
          close(unit=lun_cons)
       end if

       if (present(lun_cif)) then
          if (n_CIF_dist_text /=0) then
             write(unit=lun_cif, fmt='(a)') "loop_"
             write(unit=lun_cif, fmt='(a)') "   _geom_bond_atom_site_label_1"
             write(unit=lun_cif, fmt='(a)') "   _geom_bond_atom_site_label_2"
             write(unit=lun_cif, fmt='(a)') "   _geom_bond_distance"
             write(unit=lun_cif, fmt='(a)') "   _geom_bond_site_symmetry_2"
             write(unit=lun_cif, fmt='(a)') "   _geom_bond_publ_flag"

             do i=1, n_CIF_dist_text
                write(unit=lun_CIF, fmt='(a)') trim(CIF_dist_text(i))
             end do
          end if

          if (n_CIF_angl_text /=0) then
             write(unit=lun_cif, fmt='(a)') ""
             write(unit=lun_cif, fmt='(a)') "loop_"
             write(unit=lun_cif, fmt='(a)') "   _geom_angle_atom_site_label_1"
             write(unit=lun_cif, fmt='(a)') "   _geom_angle_atom_site_label_2"
             write(unit=lun_cif, fmt='(a)') "   _geom_angle_atom_site_label_3"
             write(unit=lun_cif, fmt='(a)') "   _geom_angle"
             write(unit=lun_cif, fmt='(a)') "   _geom_angle_site_symmetry_1"
             write(unit=lun_cif, fmt='(a)') "   _geom_angle_site_symmetry_3"
             write(unit=lun_cif, fmt='(a)') "   _geom_angle_publ_flag"

             do i=1, n_CIF_angl_text
              write(unit=lun_CIF, fmt='(a)') trim(CIF_angl_text(i))
             end do
          end if
       end if

    End Subroutine Calc_Dist_Angle_Sigma

    !!----
    !!---- Module Subroutine Distance_and_Sigma(Cellp,DerM,x0,x1,s0,s1,dis,s)
    !!----    Type(Cell_G_Type),         intent(in)  :: Cellp         ! Cell object
    !!----    real(kind=cp), dimension(3,3,6), intent(in)  :: DerM          ! Matrix of derivatives of Cellp%Cr_Orth_cel
    !!----    real(kind=cp), dimension(3),     intent(in)  :: x0,x1,s0,s1   ! Two points in fractional coordinates and sigmas
    !!----    real(kind=cp),                   intent(out) :: dis,s         ! Distance and sigma
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine Distance_and_Sigma(Cellp,DerM,x0,x1,s0,s1,dis,s)
       !---- Arguments ----!
       Type(Cell_G_Type),               intent(in)  :: Cellp         ! Cell object
       real(kind=cp), dimension(3,3,6), intent(in)  :: DerM          ! Matrix of derivatives of Cellp%Cr_Orth_cel
       real(kind=cp), dimension(3),     intent(in)  :: x0,x1,s0,s1   ! Two points in fractional coordinates and sigmas
       real(kind=cp),                   intent(out) :: dis,s         ! Distance and sigma

       !---- Local variables ----!
       integer                     :: i
       real(kind=cp), dimension(3) :: xc,xf
       real(kind=cp), dimension(6) :: dc,df

       xf=x1-x0
       xc = matmul(cellp%Cr_Orth_cel,xf)
       dis=sqrt(dot_product(xc,xc))
       do i=1,6
          dc(i) = dot_product(xc,matmul(DerM(:,:,i),xf))
       end do
       do i=1,3
          df(i) = dot_product(xc,Cellp%Cr_Orth_cel(:,i))
       end do
       df(4:6) =-df(1:3)
       s=0.0
       do i=1,3
          s = s + (dc(i)*Cellp%scell(i))**2
          s = s + (dc(i+3)*Cellp%sang(i)*to_rad)**2
          s = s + (df(i)*s1(i))**2 + (df(i+3)*s0(i))**2
       end do
       s=sqrt(s)/dis

    End Subroutine Distance_and_Sigma

    !!----
    !!---- Module Subroutine P1_Dist(Dmax, Cell, Spg, Ac, Lun)
    !!----    real(kind=cp),        intent(in)    :: dmax      !  In -> Max. Distance to be calculated
    !!----    type (Cell_G_Type),   intent(in)    :: Cell      !  In -> Object of Cell_G_Type
    !!----    Class(SpG_Type),      intent(in)    :: SpG       !  In -> Object of SpG_Type
    !!----    type (Atm_Cell_Type), intent(in out):: Ac        !  In -> Object of Atm_Cell_Type
    !!----                                                           Out -> Updated Object of Atm_Cell_Type
    !!----    integer,optional,     intent(in)    :: lun       !  In -> Logical Unit for writing
    !!----
    !!----    Subroutine calculate distances, below the prescribed distances "dmax",
    !!----    without standard deviations. No symmetry is applied: only lattice translations.
    !!----    Need as input the objects "Cell" (of type Cell_G_Type), "SpG" (of type SpG_Type)
    !!----    and "Ac" (or type Atoms_Cell). Complete the construction of Ac.
    !!----    Control for error is present.
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine P1_Dist(Dmax, Cell, Spg, Ac, Lun)
       !---- Arguments ----!
       real(kind=cp),            intent(in)       :: dmax
       type (Cell_G_Type),       intent(in)       :: Cell
       Class(SpG_Type),          intent(in)       :: SpG
       type (Atm_Cell_Type),     intent(in out)   :: Ac
       integer, optional,        intent(in)       :: lun

       !---- Local Variables ----!
       logical                                :: iprint
       character(len=6 )                      :: nam,nam1
       character(len=40)                      :: transla
       character(len=90)                      :: form1,form2="(a,2i4,a,a,a,a,a,f10.4,a,t62,3F8.4)"
       integer                                :: i,k,lk,i1,i2,i3,jl,nn,L,inew,ne,id
       integer, dimension(3)                  :: ic1,ic2
       integer, dimension(Ac%nat,Ac%nat)      :: mn  !neighbouring matrix
       real(kind=cp)                          :: T,dd
       real(kind=cp), dimension(3)            :: xx,x1,xo,Tn,xr, QD
       real(kind=cp), dimension(3,0:SpG%Num_Lat)            :: Ltr
       real(kind=cp), dimension(3,Ac%nat*Ac%nat*spg%multip) :: u

       iprint=.false.
       if (present(lun)) then
          if (lun > 0) iprint=.true.
       end if
       call clear_error()
       id=3*nint(0.74048*(dmax/1.1)**3)
       ltr=0.0
       if(Spg%Num_lat > 0) then
          ltr(:,1:Spg%Num_lat)=Spg%Lat_tr(1:3,1:Spg%Num_lat)
       end if

       qd(:)=1.0/cell%rcell(:)
       ic2(:)= nint(dmax/cell%cell(:)+3.0)
       ic1(:)=-ic2(:)
       mn(:,:) = 0
       inew=0
       do i=1,ac%nat
          xo(:)=Ac%xyz(:,i)
          nam= Ac%Lab(i)
          if (iprint) then
             write(unit=lun,fmt="(/,/,a)")"    -------------------------------------------------------------------"
             write(unit=lun,fmt="(a,f8.4,a,a,3f8.4)")   &
                       "    Distances less than",dmax,"  to atom: ",nam, xo(:)
             write(unit=lun,fmt="(a,/,/)")"    -------------------------------------------------------------------"
             write(unit=lun,fmt="(/,/,a,/,/)") &
                       " Orig. extr.                    Distance     tx   ty   tz       x_ext   y_ext   z_ext"
          end if
          ne=0
          do k=1,Ac%nat
             lk=1
             u(:,lk)=xo(:)
             xx(:)=Ac%xyz(:,k)
             nam1= Ac%Lab(k)
             do i1=ic1(1),ic2(1)
                do i2=ic1(2),ic2(2)
                   do i3=ic1(3),ic2(3)
                      do_jl:do jl=0,Spg%Num_Lat
                         Tn(:)=real([i1,i2,i3])+Ltr(:,jl)
                         x1(:)=xx(:)+tn(:)
                         do l=1,3
                            t=abs(x1(l)-xo(l))*qd(l)
                            if (t > dmax) cycle do_jl
                         end do
                         do nn=1,lk
                            if (sum(abs(u(:,nn)-x1(:)))  <= epsi) cycle do_jl
                         end do
                         xr = matmul(cell%cr_orth_cel,x1-xo)
                         dd=sqrt(dot_product(xr,xr))
                         if (dd > dmax .or. dd < 0.001) cycle
                         lk=lk+1
                         u(:,lk)=x1(:)
                         transla= Frac_Trans_1Dig(tn)
                         if (iprint) write(unit=lun,fmt=form2)" ",i,k,"   (",nam,")-(",nam1,"):",dd,"  "//transla,x1(:)
                         mn(i,k)=mn(i,k)+1
                         ne=ne+1
                         IF (ne > id) THEN
                            Err_CFML%Ierr=1
                            Err_CFML%flag=.true.
                            Err_CFML%Msg="Too many connected atoms! in sub. P1_dist"
                            return
                         END IF
                         Ac%neighb_atom(ne,i)=k    !Pointer to the number of atom connected to i
                         Ac%distance   (ne,i)=dd   !Corresponding distance
                         Ac%trans(:,ne,i)=tn(:)    !corresponding lattice translation
                         do nn=1,inew
                            if (abs(dd-Ac%ddist(nn)) <= epsi) then
                               if (equiv_atm(nam,nam1,Ac%ddlab(nn)))  cycle do_jl
                            end if
                         end do
                         inew=inew+1
                         Ac%ddist(inew)=dd
                         Ac%ddlab(inew)=wrt_lab(nam,nam1)
                      end do do_jl
                   end do !i3
                end do !i2
             end do !i1
          end do !k
          Ac%neighb(i)=ne
       end do !i
       Ac%ndist=inew
       if (iprint) then
          write(unit=lun,fmt="(/,/,a)") " -------------------"
          write(unit=lun,fmt="(a)"  )   " Neighbouring matrix"
          write(unit=lun,fmt="(a)")     " -------------------"
          write(unit=lun,fmt="(a)")
          write(unit=form1,fmt="(a,i4,a)") "(a,",Ac%nat,"i3)"
          write(unit=lun,fmt=form1)"     ",(i,i=1,Ac%nat)
          write(unit=lun,fmt="(a)")
          write(unit=form1,fmt="(a,i4,a)") "(i3,a,",Ac%nat,"i3)"
          do i=1,ac%nat
             write(unit=lun,fmt=form1) i,"  ",(mn(i,k),k=1,Ac%nat)
          end do
          write(unit=lun,fmt="(a,/,/,/)")
       end if

    End Subroutine P1_Dist

    !!----
    !!---- Module Subroutine Print_Distances(Lun, Dmax, Cell, Spg, A)
    !!----    integer,                  intent(in)   :: lun    !  In -> Logical Unit for writing
    !!----    real(kind=cp),            intent(in)   :: dmax   !  In -> Max. Distance to be calculated
    !!----    type (Cell_G_Type),       intent(in)   :: Cell   !  In -> Object of Cell_G_Type
    !!----    Class(SpG_Type),          intent(in)   :: SpG    !  In -> Object of SpG_Type
    !!----    type (AtList_Type),       intent(in)   :: A      !  In -> Object of AtList_Type
    !!----
    !!----    Subroutine to print distances, below the prescribed distances
    !!----    "dmax", without standard deviations.
    !!----    Need as input the objects "Cell" (of type Cell_G_Type), "SpG"
    !!----    (of type SpG_Type) and "A" (or type AtList_Type, that should be
    !!----    allocated in the calling program).
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine Print_Distances(Lun, Dmax, Cell, Spg, A)
       !-- Arguments --!
       integer,             intent(in)   :: lun
       real(kind=cp),       intent(in)   :: dmax
       type (Cell_G_Type),  intent(in)   :: Cell
       Class(SpG_Type),     intent(in)   :: SpG
       type (AtList_Type),  intent(in)   :: A

       !---- Local Variables ----!
       integer                           :: i,j,k,lk,i1,i2,i3,jl,npeq,nn,L,nlines
       character(len=80), dimension(12)  :: texto=" "
       character(len=5 )                 :: nam,nam1
       character(len=40)                 :: transla
       character(len=54)                 :: form2="(a,3i4,a,a,a,a,a,f10.4,a,t66,3F8.4)" !&
                                            !"("" "",3I4,""  ("",a,"")-("",a,""):"",f9.4,""   "",a,""  "",3F8.4)"
       integer,          dimension(3)    :: ic1,ic2
       real(kind=cp),    dimension(3)    :: xx,x1,xo,Tn,xr, QD
       real(kind=cp)                     :: T,dd
       real(kind=cp),    dimension(3,A%Natoms*Spg%multip) :: uu
       real(kind=cp),    dimension(3,0:SpG%Num_Lat)       :: Lat_tr
       !real(kind=cp),    dimension(3,SpG%Multip)          :: trr
       !integer,          dimension(3,3,SpG%Multip)        :: Mat
       !
       !d=SpG%d
       !do i=1,SpG%Multip
       !  Mat(:,:,i) = SpG%Op(i)%Mat(1:3,1:3)
       !    trr(:,i) = SpG%Op(i)%Mat(1:3,d)
       !End do
       if(.not. init_symOP) call init_opMatTr(SpG)
       Lat_tr=0.0
       Lat_tr(:,1:SpG%Num_Lat)  = real(SpG%Lat_tr(1:3,1:SpG%Num_Lat))
       qd(:)=1.0/cell%rcell(:)
       ic2(:)= nint(dmax/cell%cell(:)+1.0)
       ic1(:)=-ic2(:)
       npeq=spg%numops

       if (Spg%Centred == 2) then
          npeq=2*npeq
          write(unit=lun,fmt="(a)")" => Symmetry operators combined with inversion centre:"
          nlines=1
          do i=SpG%NumOps+1,npeq
             if (mod(i,2) == 0) then
                write(unit=texto(nlines)(36:70),fmt="(a,i2,a,a)") &
                                           " => SYMM(",i,"): ",trim(SpG%Symb_Op(i))
                nlines=nlines+1
             else
                write(unit=texto(nlines)( 1:34),fmt="(a,i2,a,a)")  &
                                           " => SYMM(",i,"): ",trim(SpG%Symb_Op(i))
             end if
          end do
          do i=1,min(nlines,12)
             write(unit=lun,fmt="(a)") texto(i)
          end do
       end if

       do i=1,A%natoms
          nam=a%atom(i)%lab
          xo(:)=a%atom(i)%x(:)
          write(unit=lun,fmt="(/,/,a)")"    -------------------------------------------------------------------"
          write(unit=lun,fmt="(a,f8.4,a,a,3f8.4)")   &
                    "    Distances less than",dmax,"  to atom: ",nam, xo(:)
          write(unit=lun,fmt="(a,/,/)")"    -------------------------------------------------------------------"
          write(unit=lun,fmt="(/,/,a,/,/)") &
                    " Orig. extr. p.equiv.           Distance     tx   ty   tz       x_ext   y_ext   z_ext"
          do k=1,a%natoms
             lk=1
             uu(:,lk)=xo(:)
             nam1=a%atom(k)%lab
             do j=1,npeq
                !xx=Apply_OP(Spg%Op(j),a%atom(k)%x)
                xx=Matmul(opMat(:,:,j),a%atom(k)%x)+opTr(:,j)
                do i1=ic1(1),ic2(1)
                   do i2=ic1(2),ic2(2)
                      do i3=ic1(3),ic2(3)
                         do_jl:do jl=0,SpG%Num_Lat
                            Tn(:)=real([i1,i2,i3])+Lat_tr(1:3,jl)
                            x1(:)=xx(:)+tn(:)
                            do l=1,3
                               t=abs(x1(l)-xo(l))*qd(l)
                               if (t > dmax) cycle do_jl
                            end do
                            do nn=1,lk
                               if (sum(abs(uu(:,nn)-x1(:)))  <= epsi) cycle do_jl
                            end do
                            xr = matmul(cell%cr_orth_cel,x1-xo)
                            dd=sqrt(dot_product(xr,xr))
                            if (dd > dmax .or. dd < 0.001) cycle
                            lk=lk+1
                            uu(:,lk)=x1(:)
                            transla= Frac_Trans_1Dig(tn)
                            write(unit=lun,fmt=form2)" ",i,k,j,"   (",nam,")-(",nam1,"):",dd,"  "//transla,x1(:)
                            !write(unit=lun,fmt=form2)i,k,j,nam ,nam1,dd,transla,x1(:)
                                        !    "("" "",3I4,""  ("",a,"")-("",a,""):"",f9.4,""   "",a,""  "",3F8.4)"
                         end do do_jl
                      end do !i3
                   end do !i2
                end do !i1
             end do !j
          end do !k
       end do !i

    End Subroutine Print_Distances

 End Submodule Geom_Distances
