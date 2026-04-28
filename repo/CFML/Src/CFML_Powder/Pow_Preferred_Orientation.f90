SubModule(CFML_Powder) Pow_Preferred_Orientation
  !>
  !> Derived from FullProf Module: Multiaxial March-Model for preferred orientation
  !>
  implicit none

  Contains

    Module Subroutine Preferred_orientation(model,n_pref,axes_pref,gr,mRot,par,Rfl,pref_corr,lambda,norstep,der)
    !> Multi-Axial March-Dollase Model for Preferred Orientation
      character(len=*),              intent(in) :: model      !> MAX_MD, MAXHP_MD
      integer,                       intent(in) :: n_pref     !> Number of preferred orientation axes for multiaxial March-Dollase model
      real(kind=cp), dimension(:,:), intent(in) :: axes_pref  !> (4,n_pref) Axes for preferred orientation, forth componet is the modulus squared
      real(kind=cp), dimension(3,3), intent(in) :: gr         !> Reciprocal metric tensor
      type(rot_mat_type),            intent(in) :: mRot       !> Rotational part of symmetry operators: mRot%rot(:,:,i=1,mRot%numops)
      real(kind=cp), dimension(:,:), intent(in) :: par        !> (2,n_pref) Preferred orientation parameters (value and fraction)
      Class(Refl_Type),              intent(in) :: Rfl        !> Bragg reflection
      real(kind=cp),                 intent(out):: pref_corr  !> Preferred orientation correction factor for reflection Rfl
      real(kind=cp),       optional, intent(in) :: lambda     !> Needed for MAXHP_MD model to calculate sinth
      integer,             optional, intent(in) :: norstep    !> Number of steps for integration in model MAXHP_MD
      real(kind=cp), dimension(:,:), optional, intent(out) :: der !> derivatives w.r.t. free parameters
      !--- Local variables ---!
      integer        :: k,L, na, norstp
      real(kind=cp)  :: prec1,prec2,spak,pak,paknn,pred, pakn, predev,&
                        b,b1,b2,stp,sinth,s,sdel
      !
      real(kind=cp), dimension(n_pref) :: prec, dol, frx, ddol
      real(kind=cp), dimension(3)      :: hh


      s= 2.0 * Rfl%s  ! 2senTheta/Lambda = 1/d => modulus of the vector Rfl%h
      Select Case(model)

        Case('MAX_MD')
          do na=1,n_pref
            dol(na)=par(1,na)
            frx(na)=par(2,na)
          end do
          frx(n_pref)=1.0-sum(frx(1:n_pref-1))
          do na=1,n_pref
            prec1=0.0
            prec2=0.0
            do k=1,mRot%numops
              hh=matmul(Rfl%hr,mRot%rot(:,:,k))
              pak=dot_product(axes_pref(1:3,na), matmul(gr,hh))
              pak=pak*pak/(axes_pref(4,na)*Rfl%s)            ! cos^2(a)
              spak=abs(1.0-pak)                              ! sin^2(a)
              paknn = dol(na)*dol(na)*pak+spak/dol(na)       ! r^2 cos^2(a)+sin^2(a)/r
              prec1=prec1+1.0/paknn**1.5                     ! or=1/(r^2 cos^2(a)+sin^2(a)/r)^1.5
              if(present(der)) then
                predev = 2.0*dol(na)*pak-spak/dol(na)/dol(na)  ! d(paknn)/d(dol)
                prec2=prec2+predev/paknn**2.5                  ! d(or)/d(dol)=d(or)/d(paknn)* d(paknn)/d(dol)
              end if
            end do
            if(present(der))  ddol(na)=-1.5*prec2/real(mRot%numops)                   ! d(<prec>)/d(dol)
            prec(na)=prec1/real(mRot%numops)                        ! <or>
          end do

        Case('MAXHP_MD')
         ! Multi-axial March-Dollase model for preferred orientation (high pressure anvil cells)
         ! j. synchrotron rad. (1996). 3, 112-119
         ! "observation and modelling of preferred orientation in two-dimensional powder patterns"
         ! n. g. wright, r. j. nelmes, s. a. belmonte and m. i. mcmahon
         ! pod: preferred orientation direction (crystallographic direction),
         ! poa: preferred orientation axis (sample/environment axis)
         !  Crystallites tend to aling their pod along the poa
         !  ib: incident beam, db: diffracted beam, alpha: angle of pod^poa for a particular cristallite
         ! psi: angle between pod and scattering vector
         !
         ! integer,   dimension(npht,npatt)  :: naxes   ! number of preferred orientation directions (<=5)
         ! integer,   dimension(npht,npatt)  :: norstp  ! number of steps for integration preferred orientation correction
         !
         !    <p(alpha)>hkl = 1/2pi integral[0,2pi]{ [sin^2(alpha)/r + r^2 cos^2(alpha)]^(-3/2)} d(delta) <expression 4>
         !
         !    cos(alpha) = cos(theta)sin(psi)sin(delta) - sin(theta)cos(psi) <expression 6>
         !    squaring and taking into account that the cross term integrates to zero, we have to use
         !        b(alpha)=cos^2(theta)sin^2(psi)sin^2(delta) + sin^2(theta)cos^2(psi)
         !    and calculate:
         !
         !    <p(alpha)>hkl = 1/2pi integral[0,2pi]{ [(1-b(delta))/r + r^2 b(delta)]^(-3/2)} d(delta) <expression 4>
         !
          norstp=15
          if(present(norstep)) norstp=norstep
          if(present(lambda)) sinth=Rfl%s*lambda
          stp=TPI/real(norstp)

          do na=1,n_pref
            dol(na)=par(1,na)
            frx(na)=par(2,na)
          end do
          frx(n_pref)=1.0-sum(frx(1:n_pref-1))

          do na=1,n_pref
            prec1=0.0
            prec2=0.0

            do k=1,mRot%numops
              hh=matmul(Rfl%hr,mRot%rot(:,:,k))
              pak=dot_product(axes_pref(1:3,na), matmul(gr,hh))
              pak=pak*pak/(axes_pref(4,na)*Rfl%s)            ! cos^2(psi)
              spak=abs(1.0-pak)                              ! sin^2(psi)
              b1= abs(1.0 - sinth*sinth)*spak
              b2= sinth*sinth*pak
              paknn=0.0

              predev=0.0
              do l=1,norstp
                sdel= sin(real(l)*stp)
                b=b1*sdel*sdel + b2
                pakn= abs(1.0-b)/dol(na) + dol(na)*dol(na)* b
                paknn=paknn+ (pakn)**(-1.5)                       ! integ { 1/(r^2 cos^2(a)+sin^2(a)/r)^1.5}
                if(present(der)) then
                  pred = 2.0*dol(na)*b-abs(1.0-b)/dol(na)/dol(na)   ! d(pakn)/d(dol)
                  predev=predev+pred/pakn**2.5
                end if
              end do
              paknn = paknn/real(norstp)
              prec1=prec1+paknn
              if(present(der)) then
                predev=predev/real(norstp)
                prec2=prec2+predev                         ! d(or)/d(dol)=d(or)/d(paknn)* d(paknn)/d(dol)
              end if
            end do
            prec(na)=prec1/real(mRot%numops)                        ! <or>
            if(present(der)) ddol(na)=-1.5*prec2/real(mRot%numops)  ! d(<prec>)/d(dol)
          end do
      End Select

      pref_corr=0.0
      do na=1,n_pref
        pref_corr=pref_corr + prec(na) * frx(na)
      end do

      if(present(der)) then
        do na=1,n_pref
          der(1,na)=ddol(na)*frx(na)
          der(2,na)=prec(na)-prec(n_pref)
        end do
      end if

    End Subroutine Preferred_orientation

End SubModule Pow_Preferred_Orientation