 SubModule (CFML_SXTAL_Geom) SXTAL_UB

  implicit none

  Contains

    !!----
    !!---- Module Subroutine cell_fr_UB(ub,ipr,dcel,rcel)
    !!---- real(kind=cp),Dimension(3,3),         Intent(In)  :: ub
    !!---- Integer, optional,                    Intent(In)  :: ipr
    !!---- real(kind=cp),Dimension(6), optional, Intent(out) :: dcel,rcel
    !!----
    !!----    Calculate and print cell parameters from UB-matrix
    !!----
    !!---- Update: May 2011, June 2020
    !!
    Module Subroutine cell_fr_UB(ub,ipr,dcel,rcel)
       !---- Arguments ----!
       real(kind=cp),Dimension(3,3),         Intent(In)  :: ub
       Integer, optional,                    Intent(In)  :: ipr
       real(kind=cp),Dimension(6), optional, Intent(out) :: dcel,rcel

       !--- Local Variables ---!
       real(kind=dp), Dimension(3,3) :: g,ubinv,angle
       real(kind=cp), Dimension(3,3) :: sg
       real(kind=dp), Dimension(3)   :: acal,angcal,cala,calang
       integer                       :: i,j,k,jn,kn
       real(kind=dp)                 :: x

       call clear_error()
       g=Matmul(Transpose(ub),ub)
       sg=g
       !..inverse matrix g=b'b  to get direct cell parameters
       !..On first run through loop calculation of reciprocal cell, second do real cell
       Do k=1,2
           If(k==2) sg=invert(sg)
           if (err_cfml%ierr /= 0) return
           g=sg
           Do  i=1,3
             acal(i)=Sqrt(g(i,i))
           End Do
           Do  i=1,3
             j=i
             jn=Mod(j,3)+1
             kn=Mod(jn,3)+1
             angcal(i)=acosd(g(jn,kn)/(acal(jn)*acal(kn)))
           End Do
           If(k==2) Exit
           cala   = acal   !store reciprocal latice in the first pass
           calang = angcal
       End Do
       if(present(dcel)) then
         dcel(1:3)=acal
         dcel(4:6)=angcal
       end if
       if(present(rcel)) then
         rcel(1:3)=cala
         rcel(4:6)=calang
       end if
       if(present(ipr)) then
         !.....Now invert UB to obtain the hkl's along the orthogonal diffractometer axes
         ubinv=invert(ub)
         if (err_cfml%ierr /= 0) return
         Write(Unit=ipr,Fmt="(/,a)")                " => Parameters deduced from the UB matrix "
         Write(Unit=ipr,Fmt="(a,3f12.5,tr4,3f9.4)") " => Direct     cell dimensions: ",acal,angcal
         Write(Unit=ipr,Fmt="(a,3f12.8,tr4,3f9.4)") " => Reciprocal cell dimensions: ",cala,calang
         Write(Unit=ipr,Fmt="(/,a)")" =>               UB-Matrix                                          Inverse of UB-Matrix "
         Write(Unit=ipr,Fmt="(a)")    "           A*            B*            C*                 X(PH=0,CH=0)  Y(PH=90,CH=0)    Z(CHI=90)"
         Write(Unit=ipr,Fmt="(a,3f14.8,a,3f14.8)") "  X",ub(1,:),"          H",ubinv(1,:)
         Write(Unit=ipr,Fmt="(a,3f14.8,a,3f14.8)") "  Y",ub(2,:),"          K",ubinv(2,:)
         Write(Unit=ipr,Fmt="(a,3f14.8,a,3f14.8)") "  Z",ub(3,:),"          L",ubinv(3,:)
         !.....Now calculate angles between recip axes and orthogonal diffract. axes
         Do  i=1,3
           Do  j=1,3
             x = ub(i,j)/cala(j)
             If (x > 1.0) Then
                Write (ipr,"(a,3e12.4)") " Error x >1.0! Values of x,ub,acal: ",x,ub(i,j),acal(i)
             Else
               angle(i,j) = acosd(x)
             End If
           End Do
         End Do
         Write(Unit=ipr,Fmt="(/,a)")"    With all diffractometer angles set to 0, the angles between the "
         Write(Unit=ipr,Fmt="(a)")  "    reciprocal (A*,B*,C*) and diffractometer (X,Y,Z) axes are ..."
         Write(Unit=ipr,Fmt="(a,3f12.4,a)") "    (A*-X  B*-X  C*-X)  (",angle(1,:),")"
         Write(Unit=ipr,Fmt="(a,3f12.4,a)") "    (A*-Y  B*-Y  C*-Y)  (",angle(2,:),")"
         Write(Unit=ipr,Fmt="(a,3f12.4,a)") "    (A*-Z  B*-Z  C*-Z)  (",angle(3,:),")"
       end if
    End Subroutine cell_fr_UB

    !!----
    !!---- Module Function genb(c) Result (b)
    !!----    Type(Cell_G_Type), Intent(In)   :: c
    !!----    real(kind=cp), Dimension(3,3)   :: b
    !!----
    !!--<<   Calculation of [B] matrix
    !!----   Busing&Levy Acta Cryst.(1967)22,457-464  Equation 3
    !!----   Wooster R. Sci. Instrum. (1962)39,103
    !!----      C%cell  : Direct cell parameters
    !!----      C%rcell : Reciprocal cell parameters
    !!----      C%ang   : Direct cell angles
    !!-->>      C%rcell : Reciprocal cell angles
    !!----
    !!---- Update: April 2008, June 2020
    !!
    Module Function genb(c) Result (b)
       !---- Arguments ----!
       Type(Cell_G_Type), Intent(In)  :: c
       real(kind=cp), Dimension(3,3)  :: b

       b(1,1)=c%rcell(1)
       b(1,2)=c%rcell(2)*cosd(c%rang(3))
       b(1,3)=c%rcell(3)*cosd(c%rang(2))
       b(2,2)=c%rcell(2)*sind(c%rang(3))
       b(2,3)=-(c%rcell(3)*sind(c%rang(2))*cosd(c%ang(1)))
       b(3,3)=1.0_cp/c%cell(3)
       b(2,1)=0.0_cp
       b(3,1)=0.0_cp
       b(3,2)=0.0_cp
    End Function genb

    !!----
    !!---- Module Function GenUB(b,h1,h2,h1o,h2o) Result (ub)
    !!----    real(kind=cp), Dimension(3,3), Intent(In)  :: B        !Busing-Levy B-matrix
    !!----    real(kind=cp), Dimension(3),   Intent(In)  :: h1,h2    !Miller indices
    !!----    real(kind=cp), Dimension(3),   Intent(In)  :: h1o,h2o  !Components in Lab system
    !!----
    !!----    Original from   A.Filhol  25/05/84
    !!----    Given the [B] matrix, the Miller indices of two reflections, h1 & h2,
    !!----    and the components of these two reflections, h1o & h2o, in the laboratory
    !!----    system, this subroutine provides the matrix UB. Only the direction in the
    !!----    laboratory system of reflections are needed, e.g. h1o and h2o may be unitary
    !!----    vectors or whatever other vector along these directions.
    !!----
    !!----    [hc] : Reflection H,K,L in the reciprocal lattice orthonormal system
    !!----    [hc] = [B] [h]
    !!----    [ho] : Reflection H,K,L in the Cartesian laboratory system
    !!----    [ho]=[UB]*[hc]
    !!----
    !!---- Update: April 2008, June 2020
    !!
    Module Function GenUB(b,h1,h2,h1o,h2o) Result (UB)
       !---- Arguments ----!
       real(kind=cp), Dimension(3,3), Intent(In)  :: B        !Busing-Levy B-matrix
       real(kind=cp), Dimension(3),   Intent(In)  :: h1,h2    !Miller indices
       real(kind=cp), Dimension(3),   Intent(In)  :: h1o,h2o  !Components in Lab system
       real(kind=cp), Dimension(3,3)              :: UB       !Busing-Levy UB-matrix

       !--- Local Variables ---!
       real(kind=cp), Dimension(3)  :: h1c,h2c,v1,v2
       real(kind=cp), Dimension(3,3):: trpc,trpo,u

       call clear_error()
       !Calculation of the reciprocal components in the cartesian reciprocal axes
       h1c=Matmul(B,h1)
       h2c=Matmul(B,h2)
       v1=h1o
       v2=h2o
       Call triple(h1c,h2c,trpc) !Orthonormal frame attached to h1c,h2c
       If (err_CFML%Ierr /= 0) Then
          ub=0.0_cp
          Return
       End If
       Call triple(v1,v2,trpo) !Orthonormal frame attached to h1o,h2o
       If (err_CFML%Ierr /= 0) Then
          ub=0.0_cp
          Return
       End If
       !..... MATRIX [U]  *** Equation 27 (B-L)
       U=Matmul(trpo,Transpose(trpc))

       !..... MATRIX [UB]
       UB=Matmul(U,B)
    End Function GenUB

    !!---- Module Function Get_UB_from_hkl_hkl_omega(wave,Cell,h1,h2,omega) Result(UB)
    !!----   real(kind=cp),                 intent(in)    :: wave  !Vavelength
    !!----   type (Cell_G_Type),            intent(in)    :: Cell  !Unit cell object
    !!----   real(kind=cp), dimension(3),   intent(in)    :: h1    !Indices of the known first reflection in plane
    !!----   real(kind=cp), dimension(3),   intent(in)    :: h2    !Indices of the known second reflection in plane
    !!----   real(kind=cp),                 intent(in)    :: omega !Value of the omega motor for the second reflection (vertical spindle)
    !!----   real(kind=cp), dimension(3,3)                :: UB    !Generated Busing-Levy UB-matrix
    !!----
    !!----   This subroutine generates a UB matrix when two reflections in the horizontal plane
    !!----   are known (indices hkl and h'h'l') and the second reflection has been measured and
    !!----   its omega angle is known.
    !!----
    !!----   Updated: June-2012 (JRC), June 2020
    !!----
    Module Function Get_UB_from_hkl_hkl_omega(wave,Cell,h1,h2,omega) Result(UB)
      real(kind=cp),                 intent(in)    :: wave
      type (Cell_G_Type),            intent(in)    :: Cell
      real(kind=cp), dimension(3),   intent(in)    :: h1
      real(kind=cp), dimension(3),   intent(in)    :: h2
      real(kind=cp),                 intent(in)    :: omega
      real(kind=cp), dimension(3,3)                :: UB
      ! Local variables
      real(kind=cp)                 :: theta1,theta2,alpha,del_omega,d1s,d2s
      real(kind=cp), dimension(3)   :: ho1,ho2,s1,s2
      real(kind=cp), dimension(3,3) :: Rot

      call clear_error()
      if(Co_linear(h1,h2,3)) then
        Err_CFML%Ierr=1
        Err_CFML%Msg="The two provided reflections are co-linear, no UB-matrix can be calculated"
        return
      end if
      !
      !Determination of the Bragg angles of two reflections in the scattering plane
      !
      d1s=sqrt(dot_product(h1,matmul(Cell%GR,h1))) !d1*
      d2s=sqrt(dot_product(h2,matmul(Cell%GR,h2))) !d2*
      theta1=asind(0.5*wave*d1s)
      theta2=asind(0.5*wave*d2s)
      alpha=acosd(dot_product(h1,matmul(Cell%GR,h2))/d1s/d2s) !Angle between the two reciprocal vectors
      del_omega=theta1-theta2+alpha  !Variation in omega to put the first reflection in diffraction position
      !
      !Calculation of the Cartesian components of the two reflections in the scattering plane
      !
      s2=d2s*(/cosd(Theta2),-sind(Theta2),0.0_cp/)   !z4   diffraction position
      Rot= Phi_Mat(omega)
      ho2=matmul(transpose(rot),s2)                  !z1   zero motor angles
      s1=d1s*(/cosd(Theta1),-sind(Theta1),0.0_cp/)   !z4   diffraction position
      Rot= Phi_mat(omega+del_omega)
      ho1=matmul(transpose(rot),s1)                  !z1   zero motor angles
      !
      ! Generate UB-matrix
      !
      UB= GenUB(Cell%BL_M,h1,h2,ho1,ho2)
      if(Err_CFML%Ierr /= 0) then
        Err_CFML%Msg = "Error in the calculation of UB-matrix hkl_hkl_omega: "//trim(Err_CFML%Msg)
      end if

    End Function Get_UB_from_hkl_hkl_omega

    !!---- Module Function Get_UB_from_uvw_hkl_omega(wave,Cell,Zone_Axis,h1,omega) Result(UB)
    !!----   real(kind=cp),                 intent(in)    :: wave      !Vavelength
    !!----   type (Cell_G_Type),            intent(in)    :: Cell      !Unit cell object
    !!----   Type (Zone_Axis_type),         intent(in out):: Zone_Axis !Zone axis (See CFML_Crystal_Metrics module)
    !!----   real(kind=cp), dimension(3),   intent(in)    :: h1        !Indices of the known reflection in plane
    !!----   real(kind=cp),                 intent(in)    :: omega     !Value of the omega motor (vertical spindle)
    !!----   real(kind=cp), dimension(3,3)                :: UB        !Generated Busing-Levy UB-matrix
    !!----
    !!----   This subroutine generates a UB matrix when the vertical zone axis of the crystal
    !!----   is known and a reflection in the horizonal plane has been measured with known
    !!----   indices an value of the omega angle.
    !!----
    !!----   Updated: June-2012 (JRC), June 2020
    !!----
    Module Function Get_UB_from_uvw_hkl_omega(wave,Cell,Zone_Axis,h1,omega) Result(UB)
      real(kind=cp),                 intent(in)    :: wave
      type (Cell_G_Type),            intent(in)    :: Cell
      Type (Zone_Axis_type),         intent(in out):: Zone_Axis
      real(kind=cp), dimension(3),   intent(in)    :: h1
      real(kind=cp),                 intent(in)    :: omega
      real(kind=cp), dimension(3,3)                :: UB
      ! Local variables
      real(kind=cp)                  :: theta1,theta2,alpha,del_omega,d1s,d2s
      real(kind=cp), dimension(3)    :: h2,ho1,ho2,s1,s2
      real(kind=cp), dimension(3,3)  :: Rot

      call clear_error()

      !First check that the provided reflection is perpendicular to uvw
      if(dot_product(real(Zone_Axis%uvw),h1) > 0.01_cp) then
        Err_CFML%Ierr=1
        write(unit=Err_CFML%Msg,fmt="(2(a,f8.3))") "The given reflection: ",h1," is not perpendicular to: ",real(Zone_Axis%uvw)
        return
      end if
      zone_axis = Get_basis_from_uvw(1.0_cp,Zone_Axis%uvw,Cell) !Here we use a dmin=1.0 angstrom
      if(Err_CFML%Ierr /= 0) then
        Err_CFML%Msg = "Error in the calculation of reflection plane: "//trim(Err_CFML%Msg )
        return
      end if
      h2=real(zone_axis%rx)       !Second reflection in the plane
      if(Co_linear(h1,h2,3)) h2=real(zone_axis%ry)
      !
      !Determination of the Bragg angles of two reflections in the scattering plane
      !
      d1s=sqrt(dot_product(h1,matmul(Cell%GR,h1))) !d1*
      d2s=sqrt(dot_product(h2,matmul(Cell%GR,h2))) !d2*
      theta1=asind(0.5*wave*d1s)
      theta2=asind(0.5*wave*d2s)
      alpha=acosd(dot_product(h1,matmul(Cell%GR,h2))/d1s/d2s) !Angle between reciprocal vectors
      del_omega=theta2-theta1+alpha  !Variation in omega to put the second reflection in diffraction position
      !
      !Calculation of the Cartesian components of the two reflections in the scattering plane
      !
      s1=d1s*(/cosd(Theta1),-sind(Theta1),0.0_cp/)
      Rot= Phi_Mat(omega)
      ho1=matmul(transpose(rot),s1)
      s2=d2s*(/cosd(Theta2),-sind(Theta2),0.0_cp/)
      Rot= Phi_mat(omega+del_omega)
      ho2=matmul(transpose(rot),s2)
      !
      ! Generate UB-matrix
      !
      UB= GenUB(Cell%BL_M,h1,h2,ho1,ho2)
      if(Err_CFML%Ierr /= 0) then
        Err_CFML%Msg = "Error in the calculation of UB-matrix: "//trim(Err_CFML%Msg)
      end if
    End Function Get_UB_from_uvw_hkl_omega


    !!----
    !!---- Module Subroutine normal(v,ierr)
    !!----    real(kind=cp), Intent(In Out), Dimension(3)   :: v
    !!----    Integer, Intent(Out)                          :: ierr
    !!----
    !!----    Normalise vector V (in Cartesian components)
    !!----
    !!---- Update: April 2008
    !!
    Module Subroutine normal(v)
       !---- Argument ----!
       real(kind=cp), Intent(In Out), Dimension(3)   :: v

       !--- Local Variables ---!
       real(kind=cp) :: d

       d=Dot_Product(v,v)
       If (d <= 0.0_cp) Then
          Err_CFML%Ierr=-1
       Else
          Err_CFML%Ierr=0
          d=Sqrt(d)
          v=v/d
       End If
    End Subroutine normal

    !!----
    !!---- Module Subroutine refvec(vhkl,ub,vs,vz)
    !!----    real(kind=cp), Intent(In),  Dimension(3)    :: vhkl
    !!----    real(kind=cp), Intent(In),  Dimension(3,3)  :: ub
    !!----    real(kind=cp), Intent(Out), Dimension(3)    :: vs,vz
    !!----
    !!----    Calculate vs,vz as reference vectors for defining Psi=0
    !!----    The B-L convention is that Psi=0 when the reflection hkl is
    !!----    in diffraction position and the c* is in the plane defined
    !!----    by vhkl and vz (z-axis of the laboratory system) for all
    !!----    reflections except when vhkl is parallel to c* in which case
    !!----    the vector b* plays the role of c* in the above prescription.
    !!----    The vector vhkl is provided with components in the reciprocal
    !!----    lattice.
    !!----
    !!---- Update: July 2008, June 2020
    !!
    Module Subroutine refvec(vhkl,ub,vs,vz)
       !---- Arguments ----!
       real(kind=cp), Intent(In),  Dimension(3)    :: vhkl
       real(kind=cp), Intent(In),  Dimension(3,3)  :: ub
       real(kind=cp), Intent(Out), Dimension(3)    :: vs,vz

       !--- Local Variables ---!
       real(kind=cp),Dimension(3) :: hn,h0
       real(kind=cp),Dimension(3) :: h1=(/0.0_cp,0.0_cp,1.0_cp/),h2=(/0.0_cp,1.0_cp,0.0_cp/),v0=(/0.0_cp,0.0_cp,1.0_cp/)

       !---- Test if VHKL is parallel to H1
       hn=vhkl
       !Check that the vector is non-null
       if(sum(abs(hn)) < 0.0001_cp) then
         Err_CFML%Ierr=-1
         Err_CFML%Msg = " Error: Null input vector @ refvec"
         return
       else
         Err_CFML%Ierr=0
       end if
       h0=Cross_Product(hn,h1)
       If (Sum(Abs(h0)) > 0.0001_cp) Then
          h0=h1         !vhkl IS NOT parallel to c*(=h1), so h1 can be used as reference
       Else
          h0=h2         !vhkl IS parallel to c*(=h1), so h2=b* is used as reference
       End If
       vs=Matmul(ub,h0) !Put the reciprocal vector c* (or b*) in the laboratory system
       vz=v0            !Z-xis of the laboratory system
    End Subroutine refvec

    !!----
    !!---- Module Subroutine triple(v1,v2,tv)
    !!----    real(kind=cp),    Intent(In Out), Dimension(3)  :: v1,v2
    !!----    real(kind=cp),    Intent(Out),    Dimension(3,3):: tv
    !!----
    !!----    Construct orthonormal triplet matrix TV, with column vectors :
    !!----    V1, (V1 x V2) x V1, V1 x V2.
    !!----
    !!---- Update: July 2008, June 2020
    !!
    Module Subroutine triple(v1,v2,tv)
       !---- Arguments ----!
       real(kind=cp),    Intent(In Out), Dimension(3)  :: v1,v2  !they come back normalized and V2 perp. to V1
       real(kind=cp),    Intent(Out),    Dimension(3,3):: tv

       !--- Local Variables ---!
       real(kind=cp),Dimension(3) :: v3
       call clear_error()
       Call normal(v1)
       v3=Cross_Product(v1,v2)
       Call normal(v3)
       v2=Cross_Product(v3,v1)
       Call normal(v2)
       If(Err_CFML%Ierr /= 0) Return
       tv(:,1)=v1(:)
       tv(:,2)=v2(:)
       tv(:,3)=v3(:)
    End Subroutine triple

    Module Subroutine ubfrqcel(spg_id,q_user,cell_user,ub,rfac,nq_max_user,npairs_max_user,nubs_max_user,angle_min_user,rtol_user,rfac_max_user,output_file)
      !---- Arguments ----!
      character(len=*),                       intent(in)  :: spg_id          ! space group (number, symbol...)
      real,    dimension(:,:),                intent(in)  :: q_user          ! scattering vectors (3,nq)
      real,    dimension(6),                  intent(in)  :: cell_user       ! cell parameters
      real,    dimension(:,:,:), allocatable, intent(out) :: ub              ! returned ubs
      real,    dimension(:),     allocatable, intent(out) :: rfac            ! rfactors for returned ubs
      integer, optional,                      intent(in)  :: nq_max_user     ! maximum number of q-vectors
      integer, optional,                      intent(in)  :: npairs_max_user ! maximum number of pairs to be tested
      integer, optional,                      intent(in)  :: nubs_max_user   ! maximum number of ubs returned
      real,    optional,                      intent(in)  :: angle_min_user  ! minimum angle between pairs of reflections
      real,    optional,                      intent(in)  :: rtol_user       ! tolerance in reciprocal space
      real,    optional,                      intent(in)  :: rfac_max_user   ! maximum allowed value for R-factor
      character(len=*), optional,             intent(in)  :: output_file     ! full path of the output file
      !---- Local parameters ----!
      integer, parameter :: nq_max_default = 20
      integer, parameter :: npairs_max_default = 3
      integer, parameter :: nubs_max_default = 100
      real,    parameter :: angle_min_default = 40.0
      real,    parameter :: rtol_default = 0.2
      real,    parameter :: rfac_max_default = 0.2
      real,    parameter :: epsil = 0.00001

      !---- Local variables ----!
      integer :: u_out = 11
      integer :: i,j,k,ii,jj,kk,ll,m,n,ip,io,nn
      integer :: nq,nq_user,nq_max,npairs_max,nubs_max,ncandidates,npairs,nubs
      integer, dimension(:), allocatable :: indx
      integer, dimension(:,:), allocatable :: candidates,pairs
      real :: angle,angle_min,rtol,rfac_max,rfac_i,stlmin,stlmax
      real, dimension(3) :: h,h1,h1c,h2,h2c
      real, dimension(4) :: dhkl
      real, dimension(3,3) :: ub_i,ub_inv
      real(kind=dp), dimension(3,3) :: ub_i_dp,ub_inv_dp
      real, dimension(:),     allocatable :: rfac_aux ! rfactors for returned ubs
      real, dimension(:),     allocatable :: s,s_user ! sin(theta) / lambda
      real, dimension(:,:),   allocatable :: q        ! returned ubs
      real, dimension(:,:,:), allocatable :: ub_aux   ! returned ubs
      logical :: is_new
      type(cell_g_type)            :: cell
      class(spg_type), allocatable :: spg
      type(reflist_type)           :: hkl

      ! Initialize error variable
      call clear_error()

      ! Set parameters
      if (present(nq_max_user)) then
          nq_max = nq_max_user
      else
          nq_max = nq_max_default
      end if
      if (present(npairs_max_user)) then
          npairs_max = npairs_max_user
      else
          npairs_max = npairs_max_default
      end if
      if (present(nubs_max_user)) then
          nubs_max = nubs_max_user
      else
          nubs_max = nubs_max_default
      end if
      if (present(angle_min_user)) then
          angle_min = angle_min_user
      else
          angle_min = angle_min_default
      end if
      if (present(rtol_user)) then
          rtol = rtol_user
      else
          rtol = rtol_default
      end if
      if (present(rfac_max_user)) then
          rfac_max = rfac_max_user
      else
          rfac_max = rfac_max_default
      end if
      if (present(output_file)) then
          if (output_file == 'stdout') then
              u_out = 6
          else
              open(unit=u_out,file=output_file,status='unknown',iostat=io)
              if (io /= 0) then
                  err_cfml%ierr = -1
                  err_cfml%flag = .true.
                  err_cfml%msg = 'ubfrqcel: Cannot open output file '//trim(output_file)
                  if (present(output_file)) write(unit=u_out,fmt='(4x,a)') 'Error:  At least two reflections must be given'
                  return
              end if
          end if
          write(unit=u_out,fmt='(a)') ' => Input parameters:'
          write(unit=u_out,fmt='(4x,a12,1x,6f10.4)')    'CELL =',cell_user(1:6)
          write(unit=u_out,fmt='(4x,a12,1x,i8,1x,a)')   'NQMAX =',     nq_max,     '! Maximum number of q-vectors used in the search'
          write(unit=u_out,fmt='(4x,a12,1x,i8,1x,a)')   'NPAIRS_MAX =',npairs_max, '! Maximum number of reflections pairs to be tested'
          write(unit=u_out,fmt='(4x,a12,1x,i8,1x,a)')   'NUBS_MAX =',  nubs_max,   '! Maximum number of ub-matrices'
          write(unit=u_out,fmt='(4x,a12,1x,f8.4,1x,a)') 'ANGLE_MIN =', angle_min,  '! Minimum angle between selected pairs'
          write(unit=u_out,fmt='(4x,a12,1x,f8.4,1x,a)') 'RTOL =',      rtol,       '! Indexing tolerance in reciprocal space'
          write(unit=u_out,fmt='(4x,a12,1x,f8.4,1x,a)') 'RFAC_MAX =',  rfac_max,   '! Maximum allowed R-factor'
          write(unit=u_out,fmt='(/,a)') ' => Units:'
          write(unit=u_out,fmt='(4x,a)') 'Length: angstrom'
          write(unit=u_out,fmt='(4x,a)') 'Angle:  degrees'
      end if

      ! Allocation
      allocate(ub_aux(3,3,nubs_max))
      allocate(rfac_aux(nubs_max))

      ! Check that at least 2 reflectionss have been given, built q-array and set stlmin and stlmax
      if (present(output_file)) write(unit=u_out,fmt='(/,a)') ' => Building q and s arrays'
      nq_user = size(q_user,2)
      if (nq_user < 2) then
          err_cfml%ierr = -1
          err_cfml%flag = .true.
          err_cfml%msg = 'ubfrqcel: At least two reflections must be given'
          if (present(output_file)) then
            write(unit=u_out,fmt='(4x,a)') 'Error:  At least two reflections must be given'
            close(unit=u_out)
          end if
          return
      end if
      allocate(indx(nq_user),s_user(nq_user))
      do i = 1 , nq_user
          s_user(i) = sqrt(q_user(1,i)**2 + q_user(2,i)**2  + q_user(3,i)**2) * 0.5
      end do
      indx = sort(s_user,nq_user)
      if (nq_user > nq_max) then
          nq = nq_max
      else
          nq = nq_user
      end if
      allocate(q(3,nq),s(nq))
      do i = 1 , nq
          q(:,i) = q_user(:,indx(i))
          s(i) = s_user(indx(i))
      end do
      deallocate(indx)
      stlmin = s(1) - rtol
      stlmax = s(nq) + rtol
      if (present(output_file)) then
          write(unit=u_out,fmt='(4x,a)') 'List of scattering vectors that will be used in the search'
          write(unit=u_out,fmt='(4x,a6,3a12,4x,a21)') 'N','q_x','q_y','q_z','s = sin(theta)/lambda'
          do i = 1 , nq
              write(unit=u_out,fmt='(4x,i6,3f12.6,4x,f21.6)') i,q(:,i),s(i)
          end do
      end if

      ! Generate reflections
      if (present(output_file)) write(unit=u_out,fmt='(/,a)') ' => Generating reflections'
      call set_crystal_cell(cell_user(1:3),cell_user(4:6),cell)
      if (err_cfml%ierr == 0) call set_spacegroup(spg_id,spg)
      if (err_cfml%ierr == 0) call hkl_gen_sxtal(cell,spg,stlmin,stlmax,hkl)
      if (present(output_file)) then
          if (err_cfml%ierr == 0) then
              write(unit=u_out,fmt='(4x,2a)') 'Space group: ',spg%spg_symb
              write(unit=u_out,fmt='(4x,a,1x,i6)') 'Number of generated reflections:',hkl%nref
          else
              write(unit=u_out,fmt='(4x,2a)') 'Error: ',trim(err_cfml%msg)
          end if
      end if

      ! Set reflections that can be indexed within rtol
      if (err_cfml%ierr == 0) then
          if (present(output_file)) write(unit=u_out,fmt='(/,a)') ' => Setting candidates'
          ! Candidates stores for each q-vector, the reflections in hkl that can index it
          ncandidates = 0
          allocate(candidates(0:hkl%nref,1:nq))
          do i = 1 , nq
              candidates(0,i) = 0 ! Column zero stores the number of reflections that can index q-vector i
              do j = 1 , hkl%nref
                  if (abs(s(i) - hkl%ref(j)%s) > rtol) cycle
                  ! Reflection j is candidate for q vector i
                  candidates(0,i) = candidates(0,i) + 1
                  candidates(candidates(0,i),i) = j
              end do
              if (candidates(0,i) > 0) ncandidates = ncandidates + 1
          end do
          if (ncandidates < 2) then
              err_cfml%ierr = -1
              err_cfml%flag = .true.
              err_cfml%msg = 'ubfrqcel: Number of indexed reflections is less than two. UB matrix cannot be determined.'
              if (present(output_file)) then
                write(unit=u_out,fmt='(4x,a)') 'Error: Number of indexed reflections is less than two. UB matrix cannot be determined.'
                close(unit=u_out)
              end if
              return
          end if
          if (present(output_file)) then
              write(unit=u_out,fmt='(4x,a)') 'List of reflections that can be indexed: '
              write(unit=u_out,fmt='(4x,a6,4a12,3a6,a12)') 'N','q_x','q_y','q_z','s','H','K','L','s'
              do i = 1 , nq
                  if (candidates(0,i) > 0) then
                      write(unit=u_out,fmt='(4x,i6,4f12.6)') i,q(:,i),s(i)
                      do j = 1 , candidates(0,i)
                          k = candidates(j,i)
                          write(unit=u_out,fmt='(58x,3i6,f12.6)') hkl%ref(k)%h,hkl%ref(k)%s
                      end do
                  end if
              end do
          end if
      end if

      ! Select pairs of reflections
      if (err_cfml%ierr == 0) then
          if (present(output_file)) write(unit=u_out,fmt='(/,a)') ' => Selecting pairs'
          allocate(pairs(2,npairs_max))
          npairs = 0
          do i = 1, nq
              if (candidates(0,i) > 0) then
                  pairs(1,npairs+1) = i
                  do j = i + 1 , nq
                      if (candidates(0,j) > 0) then
                          ! Angle between i,j must be > angle_pair
                          angle = acosd(dot_product(q(1:3,i),q(1:3,j)) / (4 * s(i) * s(j)))
                          if (angle > angle_min) then
                              pairs(2,npairs+1) = j
                              npairs = npairs + 1
                              exit
                          end if
                      end if
                  end do
                  if (npairs == npairs_max) then
                      if (present(output_file)) write(unit=u_out,fmt='(4x,a)') 'Maximum number of pairs reached.'
                      exit
                  end if
              end if
          end do
          if (npairs == 0) then
              err_cfml%ierr = -1
              err_cfml%flag = .true.
              err_cfml%msg = 'ubfrqcel: No valid pair found for testing cell'
              if (present(output_file)) then
                write(unit=u_out,fmt='(4x,a)') 'Error: No valid pair found for testing cell.'
                close(unit=u_out)
              end if
              return
          end if
      end if

      ! Test pairs
      if (err_cfml%ierr == 0) then
          if (present(output_file)) write(unit=u_out,fmt='(/,a)') ' => Testing pairs'
          nubs = 0
          do ip = 1 , npairs
              i = pairs(1,ip)
              j = pairs(2,ip)
              if (present(output_file)) write(unit=u_out,fmt='(4x,a,i3,1x,i3)') 'Pair',i,j
              do n = 1 , candidates(0,i)
                  h1 = hkl%ref(candidates(n,i))%h  ! Miller index
                  h1c = q(1:3,i)                   ! h1 in cartesian coordinates
                  do m = 1 , candidates(0,j)
                      h2  = hkl%ref(candidates(m,j))%h ! Miller index
                      h2c = q(1:3,j)                   ! h2 in cartesian coordinates
                      ub_i = GenUB(cell%BL_M,h1,h2,h1c,h2c)
                      if (err_cfml%ierr /= 0) then
                          write(unit=u_out,fmt='(8x,a,3f4.0,a,3f4.0)' ) 'Failed to build  UB for H1 = ',h1,', H2 = ',h2
                          call clear_error()
                          cycle
                      end if
                      ub_i_dp = ub_i
                      ub_inv_dp = invert_ub_dp(ub_i_dp)
                      ub_inv = ub_inv_dp
                      if (err_cfml%ierr /= 0) then
                        if (present(output_file)) then
                            write(unit=u_out,fmt='(8x,a,3f4.0,a,3f4.0)' ) 'Failed to invert UB for H1 = ',h1,', H2 = ',h2
                            call clear_error()
                            cycle
                        end if
                      end if
                      ! Compute rfac
                      rfac_i = 0.0
                      do ii = 1 , nq_user
                          h = matmul(ub_inv,q_user(1:3,ii))
                          dhkl(1:3) = abs(h - nint(h))
                          dhkl(4) = sqrt(dhkl(1)**2 + dhkl(2)**2+dhkl(3)**2)
                          rfac_i = rfac_i + dhkl(4)
                      end do
                      rfac_i = rfac_i / nq_user
                      if (present(output_file)) &
                          write(unit=u_out,fmt='(8x,a,3f4.0,a,3f4.0,4x,a,f10.4)' ) 'H1 = ',h1,', H2 = ',h2,'Rfac = ',rfac_i
                      if (rfac_i < rfac_max) then
                          ! Check if the UB matrix is new
                          is_new = .true.
                          do ll = 1 , nubs
                              if (is_new) then
                                  is_new = .false.
                                  do jj = 1 , 3
                                      do kk = 1 , 3
                                        if (abs(ub_i(jj,kk)-ub_aux(jj,kk,ll)) > epsil) is_new = .true.
                                      end do
                                  end do
                              end if
                          end do
                          if (is_new) then
                              if (nubs < nubs_max) then
                                  nubs = nubs + 1
                                  ub_aux(:,:,nubs) = ub_i(:,:)
                                  rfac_aux(nubs) = rfac_i
                              else
                                  k = maxloc(rfac_aux,1)
                                  if (rfac_i < rfac_aux(k)) then
                                      ub_aux(:,:,k) = ub_i(:,:)
                                      rfac_aux(k) = rfac_i
                                  end if
                              end if
                          end if
                      end if
                  end do
              end do
          end do
          if (nubs == 0) then
              err_cfml%ierr = -1
              err_cfml%flag = .true.
              err_cfml%msg = 'ubfrqcel: No UB-matrix found. Increase rtol or rfac.'
              if (present(output_file)) then
                write(unit=u_out,fmt='(4x,a)') 'Error: No UB-matrix found. Increase rtol or rfac.'
                close(unit=u_out)
              end if
              return
          end if
      end if

      ! Order cells according to rfac
      if (err_cfml%ierr == 0) then
          allocate(indx(nubs),rfac(nubs),ub(3,3,nubs))
          indx = sort(rfac_aux(1:nubs),nubs)
          do i = 1 , nubs
              rfac(i) = rfac_aux(indx(i))
              ub(:,:,i) = ub_aux(:,:,indx(i))
          end do
          if (present(output_file)) then
              write(unit=u_out,fmt='(a)') ' => Best UB matrix found:'
              do i = 1 , 3
                  write(unit=u_out,fmt='(6x,3f14.8)') ub(i,1:3,1)
              end do
              write(unit=u_out,fmt='(4x,a,f8.3)') 'R-Fac: ',rfac(1)
          end if
      end if

      if (present(output_file)) close(unit=u_out)

  end subroutine ubfrqcel

  module function invert_UB_dp(A) result(Ainv)

    real(kind=dp) :: A(3,3), Ainv(3,3)
    real(kind=dp) :: det

    ! Calcular el determinante
    det = A(1,1)*(A(2,2)*A(3,3) - A(2,3)*A(3,2)) &
        - A(1,2)*(A(2,1)*A(3,3) - A(2,3)*A(3,1)) &
        + A(1,3)*(A(2,1)*A(3,2) - A(2,2)*A(3,1))

    if (abs(det) < 1.0d-12) then
        err_CFML%Ierr = 1
        err_CFML%Msg = "Singular matrix"
        return
    end if

    ! General formula for a 3x3 matrix
    Ainv(1,1) =  (A(2,2)*A(3,3) - A(2,3)*A(3,2)) / det
    Ainv(1,2) = -(A(1,2)*A(3,3) - A(1,3)*A(3,2)) / det
    Ainv(1,3) =  (A(1,2)*A(2,3) - A(1,3)*A(2,2)) / det

    Ainv(2,1) = -(A(2,1)*A(3,3) - A(2,3)*A(3,1)) / det
    Ainv(2,2) =  (A(1,1)*A(3,3) - A(1,3)*A(3,1)) / det
    Ainv(2,3) = -(A(1,1)*A(2,3) - A(1,3)*A(2,1)) / det

    Ainv(3,1) =  (A(2,1)*A(3,2) - A(2,2)*A(3,1)) / det
    Ainv(3,2) = -(A(1,1)*A(3,2) - A(1,2)*A(3,1)) / det
    Ainv(3,3) =  (A(1,1)*A(2,2) - A(1,2)*A(2,1)) / det

  end function

 End SubModule SXTAL_UB
