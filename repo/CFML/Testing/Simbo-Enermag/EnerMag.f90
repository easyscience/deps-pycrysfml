  !!---- Program EnerMag
  !!---- Author: J. Rodriguez-Carvajal (LLB,ILL,CSIC)
  !!----
  !!---- Program to calculate the classical magnetic energy for a set of
  !!---- k-vectors and exchange parameters. The input file is created by
  !!---  the program SIMBO.
  !!---  Test version beta-0. Isotropic exchange.
  !!---  Created in December 1994 (Juan Rodriguez-Carvajal, LLB)
  !!---  New things added in May 1997
  !!---  New things added in June 1999
  !!---  Transformed to F90 in July 1999
  !!---  Some modifications in August 2001
  !!---
  !!---  The program code is written below the module "J_k_exchange". It uses
  !!---  the external module Super_Exchange that is also used by other programs
  !----------------------------------------------------------------------------
 Module J_k_exchange
   use CFML_gSpaceGroups, only: SPG_Type
   Implicit none

   public  :: j_k, spin_conf, genj, genk  !, read_exchange
   private :: sin2p, cos2p, spin_confr, spin_confc

   integer, public, parameter :: nat=25, id=50, njo=1800000, ijo=10, nv=125000
   integer, public                           :: natcel !number of atoms/primitive cell
   real,    public, dimension        (3,nat) :: xyz    !Coordinates of atoms in the cell
   real,    public, dimension        (  nat) :: s      !Spin of atoms
   real,    public, dimension        (  id ) :: valj   !Exchange interactions of bond id
   integer, public, dimension      (nat,nat) :: nterm  !number of terms of J(k)(i,j)
   integer, public, dimension   (nat,nat,id) :: nvalj  !Pointer to the value of Jid
   real   , public, dimension (3,nat,nat,id) :: trans  !Translation associated to the bond "id"
                                                       !in the position term (i,j) of J(k)
   character (len=6), public, dimension(nat) :: rnam   !Name of the atoms

   Interface spin_conf
      Module procedure spin_confc
      Module procedure spin_confr
   End Interface

   class(SPG_Type), allocatable, public :: Grp

   Contains

    !----------------------------------------------------------------------------------------------
    !      subroutine read_exchange()
    !      end subroutine read_exchange
    !----------------------------------------------------------------------------------------------
    Subroutine j_k(rk,exchk,iex)
      real,    intent(in)     , dimension(3)       :: rk    !propagation vector
      complex, intent(out)    , dimension(nat,nat) :: exchk !J(k)
      integer, intent(out)                         :: iex   !=0 if real J(k)
      integer :: i,j,k,n
      real :: arg

      iex=0             !Imaginary part equal to zero
      Do i=1,natcel
        Do j=1,natcel
          exchk(i,j)=cmplx(0.0,0.0)
          Do n=1,nterm(i,j)
            arg=0.0
            Do k=1,3
              arg=arg+rk(k)*trans(k,i,j,n)
            End Do
            exchk(i,j)=exchk(i,j)-valj(nvalj(i,j,n))*cmplx( cos2p(-arg),sin2p(-arg)) ! -J(k) to minimize!!!
          End Do
          exchk(i,j)=exchk(i,j)*s(i)*s(j)       !Spin values absorbed in the Energy
          IF(abs(aimag(exchk(i,j))) > 1.e-04) iex=1
        End Do
      End Do

    End Subroutine j_k

    function cos2p(x) result(f_val)
       real, intent (in) :: x
       real :: f_val
       f_val=cos(6.283185307*x)
    end function cos2p

    function sin2p(x)  result(f_val)
       real, intent (in) :: x
       real :: f_val
       f_val=sin(6.283185307*x)
    end function sin2p

    subroutine spin_confc(lun,rk,energy,d)
      integer, intent(in)               :: lun
      real, intent(in),dimension(3)     :: rk
      real, intent(in)                  :: energy
      complex, intent(in),dimension(nat):: d
      real, dimension(nat)              :: rmo, phas
      integer :: i,j
      write(unit=lun,fmt="(/,a,f12.4,/,a,3f8.4,/,a,/)")  &
          " => Spin configurations of Energy: ",energy,  &
          "    And propagation vector       : ",(rk(i),i=1,3),  &
          "    can be obtained from the following expressions:"

      do i=1,natcel
        rmo(i)=abs(d(i))
        if(rmo(i) > 1.e-15) then
          phas(i)=atan2(aimag(d(i)),real(d(i)))/6.2831853
        else
          phas(i)=0.0
        end if
      end do

      do i=1,natcel
        write(unit=lun,fmt="(i4,a,a6,4f9.5)")i,"  ",rnam(i),(xyz(j,i),j=1,3),s(i)
        if(rmo(i) > 1.e-15) then
          if(phas(i) < 0.0 ) then
           write(unit=lun,fmt="(a,f7.4,a)")"               mx =  cx . cos{2pi(k*rn",phas(i),")}"
           write(unit=lun,fmt="(a,f7.4,a)")"               my =  cy . sin{2pi(k*rn",phas(i),")}"
          else
           write(unit=lun,fmt="(a,f7.4,a)")"               mx =  cx . cos{2pi(k*rn +",phas(i),")}"
           write(unit=lun,fmt="(a,f7.4,a)")"               my =  cy . sin{2pi(k*rn +",phas(i),")}"
          end if
           write(unit=lun,fmt="(a)")       "               mz =  0"
        else
           write(unit=lun,fmt="(a)")       "               m =  0    <--- atom with zero moment"
        end if
      end do

    end subroutine spin_confc

    subroutine spin_confr(lun,rk,energy,d)
      integer, intent(in)               :: lun
      real, intent(in),dimension(3)     :: rk
      real, intent(in)                  :: energy
      real, intent(in),dimension(nat)   :: d
      real, dimension(nat)              :: rmo, phas
      integer :: i,j
      write(unit=lun,fmt="(/,a,f12.4,/,a,3f8.4,/,a,/)")  &
          " => Spin configurations of Energy: ",energy,  &
          "    And propagation vector       : ",(rk(i),i=1,3),  &
          "    can be obtained from the following expressions:"

      do i=1,natcel
        if(real(d(i)) >= 0.0) then
          phas(i)=0.0
          rmo(i)=d(i)
        else
          phas(i)=0.5
          rmo(i)=-d(i)
        end if
      end do

      do i=1,natcel
        write(unit=lun,fmt="(i4,a,a6,4f9.5)")i,"  ",rnam(i),(xyz(j,i),j=1,3),s(i)
        if(rmo(i) > 1.e-15) then
          if(phas(i) < 0.0 ) then
           write(unit=lun,fmt="(a,f7.4,a)")"               Mx =  Cx . cos{2.pi.(k*Rn",phas(i),")}"
           write(unit=lun,fmt="(a,f7.4,a)")"               My =  Cy . sin{2.pi.(k*Rn",phas(i),")}"
          else
           write(unit=lun,fmt="(a,f7.4,a)")"               Mx =  Cx . cos{2.pi.(k*Rn +",phas(i),")}"
           write(unit=lun,fmt="(a,f7.4,a)")"               My =  Cy . sin{2.pi.(k*Rn +",phas(i),")}"
          end if
           write(unit=lun,fmt="(a)")       "               Mz =  0"
        else
           write(unit=lun,fmt="(a)")       "               m =  0    <--- atom with zero moment"
        end if
      end do

    end subroutine spin_confr
      !-----------------------------------------------------------------------

    Subroutine genj(nojvar,njotas,rang1,rang2,npoi,valjj)
      integer, intent(in)                     :: nojvar
      integer, intent(out)                    :: njotas
      real,    intent(in) , dimension(ijo)    :: rang1
      real,    intent(in) , dimension(ijo)    :: rang2
      integer, intent(in) , dimension(ijo)    :: npoi
      real,    intent(out), dimension(ijo,njo):: valjj

      integer, dimension(ijo) ::  nj
      real,    dimension(ijo) ::  step
      integer :: i,i1,i2,i3,i4,i5, jj

      do i=1,nojvar
        if(npoi(i) == 1) then
          step(i) = 0.0
        else
          step(i)=(rang2(i)-rang1(i))/real(npoi(i)-1)
        end if
      end do
      valjj(1:nojvar,1:njo)=0.0
      jj=0
      do i1=1,npoi(1)
        nj(1)=i1
        do i2=1,npoi(2)
          nj(2)=i2
          do i3=1,npoi(3)
            nj(3)=i3
            do i4=1,npoi(4)
              nj(4)=i4
              do i5=1,npoi(5)
                nj(5)=i5          !Increase the number of loops if ijo>5
                jj=jj+1
                do i=1,nojvar
                  valjj(i,jj)=rang1(i)+real(nj(i)-1)*step(i)
                end do
              end do
            end do
          end do
        end do
      end do
      njotas=jj

    End Subroutine genj
      !-----------------------------------------------------------------------

    Subroutine genk(lun,ln,infil,nvk,vk,iprint)
      integer, intent(in)                      :: lun, ln
      character (len=256), intent(in out)      :: infil
      integer, intent(out)                     :: nvk
      real, intent(out), dimension(3,nv)       :: vk
      integer, intent(out)                     :: iprint
      integer :: ier,i,j,k
      character (len=1)                :: ans
      integer, parameter               :: n_sk=96
      logical, dimension(n_sk)         :: good
      logical                          :: double_k
      real, dimension(3)               :: xran
      real, dimension(3,n_sk), parameter :: sk = reshape ( (/         &
          0.00000,0.00000,0.00000,  0.50000,0.00000,0.00000,  0.00000,0.50000,0.00000,  &
          0.00000,0.00000,0.50000,  0.50000,0.50000,0.00000,  0.50000,0.00000,0.50000,  &
          0.00000,0.50000,0.50000,  0.50000,0.50000,0.50000,  0.25000,0.00000,0.00000,  &
          0.00000,0.25000,0.00000,  0.00000,0.00000,0.25000,  0.25000,0.25000,0.00000,  &
          0.25000,0.00000,0.25000,  0.00000,0.25000,0.25000,  0.25000,0.25000,0.25000,  &
          0.50000,0.25000,0.25000,  0.25000,0.50000,0.25000,  0.25000,0.25000,0.50000,  &
          0.50000,0.50000,0.25000,  0.25000,0.50000,0.50000,  0.50000,0.25000,0.50000,  &
          0.50000,0.00000,0.25000,  0.00000,0.50000,0.25000,  0.50000,0.25000,0.00000,  &
          0.00000,0.25000,0.50000,  0.25000,0.00000,0.50000,  0.25000,0.50000,0.00000,  &
          1.50000,0.00000,0.00000,  0.00000,1.50000,0.00000,  0.00000,0.00000,1.50000,  &
          1.00000,0.00000,0.00000,  0.00000,1.00000,0.00000,  0.00000,0.00000,1.00000,  &
          1.00000,0.50000,0.00000,  0.00000,1.00000,0.50000,  0.00000,0.50000,1.00000,  &
          1.00000,0.00000,0.50000,  0.50000,1.00000,0.00000,  0.50000,0.00000,1.00000,  &
          0.33333,0.00000,0.00000,  0.00000,0.33333,0.00000,  0.00000,0.00000,0.33333,  &
          0.33333,0.33333,0.00000,  0.33333,0.00000,0.33333,  0.00000,0.33333,0.33333,  &
          0.33333,0.33333,0.33333,  0.33333,0.66667,0.00000,  0.66667,0.33333,0.00000,  &
          0.66667,0.66667,0.00000,  0.66667,0.00000,0.66667,  0.00000,0.66667,0.66667,  &
          0.66667,0.33333,0.66667,  0.66667,0.66667,0.33333,  0.33333,0.66667,0.66667,  &
          0.12500,0.00000,0.00000,  0.00000,0.12500,0.00000,  0.00000,0.00000,0.12500,  &
          0.12500,0.12500,0.00000,  0.12500,0.00000,0.12500,  0.00000,0.12500,0.12500,  &
          0.12500,0.12500,0.12500,  0.12500,0.25000,0.00000,  0.12500,0.00000,0.25000,  &
          0.00000,0.12500,0.25000,  0.25000,0.12500,0.00000,  0.25000,0.00000,0.12500,  &
          0.00000,0.25000,0.12500,  0.25000,0.25000,0.12500,  0.25000,0.12500,0.25000,  &
          0.12500,0.25000,0.25000,  0.12500,0.25000,0.12500,  0.12500,0.12500,0.25000,  &
          0.00000,0.50000,0.12500,  0.50000,0.00000,0.12500,  0.50000,0.12500,0.00000,  &
          0.00000,0.12500,0.50000,  0.12500,0.00000,0.50000,  0.12500,0.50000,0.00000,  &
          0.12500,0.50000,0.50000,  0.50000,0.12500,0.50000,  0.50000,0.50000,0.12500,  &
          0.50000,0.12500,0.12500,  0.12500,0.50000,0.12500,  0.12500,0.12500,0.50000,  &
          0.33333,0.50000,0.00000,  0.33333,0.00000,0.50000,  0.00000,0.33333,0.50000,  &
          0.50000,0.33333,0.00000,  0.50000,0.00000,0.33333,  0.00000,0.50000,0.33333,  &
          0.50000,0.33333,0.50000,  0.50000,0.50000,0.33333,  0.33333,0.50000,0.50000,  &
          0.33333,0.50000,0.33333,  0.33333,0.33333,0.50000,  0.50000,0.33333,0.33333   &
                     /),(/3,96/) )

      real,    dimension(3) ::  x1,x2,x3,fs,dk
      integer, dimension(3) ::  ngrid
      integer :: iop, n, l, i1,i2, i_kve=20
      iprint=0
      double_k=.false.
      good=.false.
      call random_seed()
      do
       write(unit=*,fmt="(/,a,/)")"    Vectors k in BZ, components in [-1,1]"
       write(unit=*,fmt="(a)")" => Options:"
       write(unit=*,fmt="(a)")"             0: Special k-vectors"
       write(unit=*,fmt="(a)")"             1: Give individual k-vectors"
       write(unit=*,fmt="(a)")"             2: k-vectors along a line"
       write(unit=*,fmt="(a)")"             3: k-vectors in a plane"
       write(unit=*,fmt="(a)")"             4: General + Special k-vectors"
       write(unit=*,fmt="(a)")"             5: Grid inside the Brillouin Zone"
       write(unit=*,fmt="(a)")"   "
       write(unit=*,fmt="(a)",advance="no")" => Give the option: "
       read(unit=*,fmt=*,iostat=ier) iop
        if(ier /=0) cycle
        if(iop < 0 .or. iop > 5) cycle
        exit
      end do
      Select Case (iop)

      Case (0)
        iprint=1
        nvk=n_sk
        do i=1,n_sk
          do j=1,3
            vk(j,i)=sk(j,i)
          end do
        end do
        write(unit=lun,fmt="(/,a)") " => Only special k-vectors stored in the program will be tested"
        write(unit=lun,fmt="( a)") "    These vectors are:"
         do i=1,n_sk,4
           write(unit=lun,fmt="(4(a,i2,a,3f8.4))") ("  #",i+l,":",vk(:,i+l),l=0,3)
         end do

      Case (1)
        iprint=1
        write(unit=*,fmt="(a,a,a)")  &
            " => Interactive (i) or read from ",infil(1:ln),".kve file (k): "
        read(unit=*,fmt="(a)") ans
        if(ans == "K" .or. ans == "k") then
          open(unit=i_kve,file=infil(1:ln)//".kve",status="replace",action="write")
          read(unit=i_kve,fmt=*) nvk
          do j=1,nvk
            read(unit=i_kve,fmt=*) (vk(i,j),i=1,3)
          end do
          close(unit=i_kve)
        else
         do
          write(unit=*,fmt="(a,i6,a)",advance="no")" => Number of k-vectors (<",nv,"): "
          read(unit=*,fmt=*,iostat=ier) nvk
          if(ier /=0) cycle
          exit
         end do
          do j=1,nvk
           do
            write(unit=*,fmt="(a,i2,a)",advance="no")" => K-vector No ",j,": "
            read(unit=*,fmt=*,iostat=ier) (vk(i,j),i=1,3)
            if(ier /=0) cycle
            exit
           end do
          end do
        end if

        write(unit=lun,fmt="(/,a)",advance="no") " => Selected set of k-vectors given interactively/in a file "
        write(unit=lun,fmt="(  a)") "    These vectors are:"
         do i=1,nvk,4
           write(unit=lun,fmt="(4(a,i2,a,3f8.4))") ("  #",i+l,":",vk(:,i+l),l=0,3)
         end do

      Case (2)
         iprint=1
         do
            write(unit=*,fmt="(a,i6,a)",advance="no")" => Number of k-vectors (<",nv,"): "
            read(unit=*,fmt=*,iostat=ier) nvk
            if(ier /=0) cycle
            exit
         end do
         write(unit=*,fmt="(a)")" => Origin and extreme of the line (in r.l.u.): "
         read(unit=*,fmt=*) (x1(j),j=1,3),(x2(k),k=1,3)
         do i=1,3
           x3(i)=(x2(i)-x1(i))/real(nvk-1)
         end do
         do j=1,nvk
           do i=1,3
             vk(i,j)=x1(i)+real(j-1)*x3(i)
           end do
         end do

         write(unit=lun,fmt="(/,a)")      " => Test of k-vectors along a line "
         write(unit=lun,fmt="( a,3f8.4)") "    These vectors are between:",(x1(j),j=1,3)
         write(unit=lun,fmt="( a,3f8.4)") "                         and :",(x2(j),j=1,3)
         write(unit=lun,fmt="( a,i6   )") "    Total number of k-vectors:",nvk

      Case (3)
         iprint=1
         do
           write(unit=*,fmt="(a,i6,a)",advance="no")" => Number of k-vectors (<",nv,"): "
           read(unit=*,fmt=*,iostat=ier) nvk
           if(ier /=0) cycle
           exit
         end do
         n=SQRT(real(nvk))
         write(unit=*,fmt="(a)")  " => Plane defined by K=m1*U+m2*V  "
         write(unit=*,fmt="(a)")  "    Give the two vectors U,V (in r.l.u.) defining the plane: "
         read(unit=*,fmt=*) (x1(j),j=1,3),(x2(k),k=1,3)
         do i=1,3
           x1(i)=x1(i)/real(n-1)
           x2(i)=x2(i)/real(n-1)
         end do
         nvk=0
         do i1=1,n
           do i2=1,n
             do i=1,3
               x3(i)=real(i1-1)*x1(i)+real(i2-1)*x2(i)
               if(abs(x3(i)) > 1.0) x3(i)=sign(1.0,x3(i))
             end do
             nvk=nvk+1
             do i=1,3
               vk(i,nvk)=x3(i)
             end do
           end do
         end do

         write(unit=lun,fmt="(/,a)")      " => Test of k-vectors within a plane defined by K=m1*U+m2*V  "
         write(unit=lun,fmt="( a,3f8.4)") "     The vectors U and V are :",(x1(j),j=1,3)
         write(unit=lun,fmt="( a,3f8.4)") "                         and :",(x2(j),j=1,3)
         write(unit=lun,fmt="( a,i6   )") "    Total number of k-vectors:",nvk
         write(unit=*,fmt="(/,a)")      " => Test of k-vectors within a plane defined by K=m1*U+m2*V  "
         write(unit=*,fmt="( a,3f8.4)") "     The vectors U and V are :",(x1(j),j=1,3)
         write(unit=*,fmt="( a,3f8.4)") "                         and :",(x2(j),j=1,3)
         write(unit=*,fmt="( a,i6   )") "    Total number of k-vectors:",nvk

      Case (4)
         do
           write(unit=*,fmt="(a,i6,a)",advance="no")" => Number of k-vectors (<",nv,"): "
           read(unit=*,fmt=*,iostat=ier) nvk
           if(ier /=0) cycle
           exit
         end do
         Select Case (Grp%Laue)
            Case("-1")
               x1(1)=-0.50; x1(2)=-0.50; x1(3)=0.0
               x2=0.5
               if(Grp%SPG_lat == "C" .or. Grp%SPG_lat == "A" .or. Grp%SPG_lat == "B") then
                   x1(1)=-1; x1(2)=-1
                   x2(1)= 1; x2(2)= 1
               else if(Grp%SPG_lat == "I" .or. Grp%SPG_lat == "F" ) then
                   x1(1)=-1; x1(2)=-1
                   x2=1.0
               end if
            Case("2/m")
               x1(1)=-0.50; x1(2)=-0.50; x1(3)=0.0
               x2=0.5
               if(Grp%SPG_lat == "C" .or. Grp%SPG_lat == "A" .or. Grp%SPG_lat == "B") then
                   x1(1)=-1; x1(3)=-1
                   x2(1)= 1; x2(3)= 1
               else if(Grp%SPG_lat == "I" .or. Grp%SPG_lat == "F" ) then
                   x1(1)=-1; x1(3)=-1
                   x2(3)=1.0
               end if
            Case("mmm")
               x1=0.0
               x2=0.5
               if(Grp%SPG_lat == "C" .or. Grp%SPG_lat == "A" .or. Grp%SPG_lat == "B") then
                   x2(1)= 1; x2(2)= 1
               else if(Grp%SPG_lat == "I" .or. Grp%SPG_lat == "F" ) then
                   x2(3)=1.0
               end if
            Case("4/mmm")
               x1=0.0
               x2=0.5
               if(Grp%SPG_lat == "I" .or. Grp%SPG_lat == "F" ) then
                   x2(3)=1.0
               end if
            Case("3m1")
               x1=0.0
               x2=0.5
               if(Grp%SPG_lat == "R") then
                   x2(3)=1.5
               end if
            Case("6/mmm")
               x1=0.0
               x2=0.5
            Case("m3m")
               x1=0.0
               x2=0.5
               if(Grp%SPG_lat == "I" .or. Grp%SPG_lat == "F" ) then
                   x2(3)=1.0
               end if
            Case default
              write(unit=*,fmt="(a,/,a)",advance="no")" => Give region of k-vectors (in r.l.u.)",  &
                         "              (kx1,kx2,ky1,ky2,kz1,kz2): "
              read(unit=*,fmt=*) (x1(i),x2(i),i=1,3)
         End Select
         write(unit=*,fmt="(a)") " => Range of k-components:"
         do i=1,3
           write(unit=*,fmt="(a,i1,a,2(f6.2,a))")  "      k_",i," [",x1(i),",",x2(i)," ]"
           if(x1(i) < -0.000001) then
              double_k=.true.
              i1=i
           end if
         end do
         l=0
         do_spec: do i=1,n_sk
           do j=1,3
             if(sk(j,i) < x1(j) .or. sk(j,i) > x2(j)) cycle do_spec
           end do
           good(i)=.true.
           l=l+1
           do j=1,3
             vk(j,l)=sk(j,i)
           end do
         end do do_spec

         !i2=n_sk
         i2=l
         if(double_k) then
           do i=2,n_sk
             if(good(i)) then
               i2=i2+1
               vk(:,i2)=sk(:,i)
               vk(i1,i2)=-vk(i1,i2)
             end if
           end do
         end if

         do j=i2+1,nvk
             call random_number(xran)
             vk(:,j)=x1(:)+(x2(:)-x1(:))*xran(:)
         end do
         nvk=nvk+i2
         write(unit=lun,fmt="(/,a)")      " => k-vectors generated at random in the region:   "
         write(unit=lun,fmt="( a,2f8.4)") "    Kx :",x1(1),x2(1)
         write(unit=lun,fmt="( a,2f8.4)") "    Ky :",x1(2),x2(2)
         write(unit=lun,fmt="( a,2f8.4)") "    Kz :",x1(3),x2(3)
         write(unit=lun,fmt="(a,i6,a)")   "    Plus special",i2,"  vectors stored in the program"
         write(unit=lun,fmt="( a,i6   )") "    Total number of k-vectors (generated + special):",nvk
         write(unit=*,fmt="( a,i6   )")   "    Total number of k-vectors (generated + special):",nvk

      Case (5)
        do
         write(unit=*,fmt="(a)")" => Region for k-vectors (min,max,num.of divisions):"
         write(unit=*,fmt="(a)",advance="no")"     Give kx_min,kx_max, xdiv:"
         read(unit=*,fmt=*)  x1(1),x2(1),ngrid(1)
         write(unit=*,fmt="(a)",advance="no")"     Give ky_min,ky_max, ydiv:"
         read(unit=*,fmt=*)  x1(2),x2(2),ngrid(2)
         write(unit=*,fmt="(a)",advance="no")"     Give kz_min,kz_max, zdiv:"
         read(unit=*,fmt=*)  x1(3),x2(3),ngrid(3)
         if ((ngrid(1)+1)*(ngrid(2)+1)*(ngrid(3)+1)> nv) then
          write(unit=*,fmt="(a)") " => Too many k-vectors ... reduce the grid!"
          cycle
         else
          exit
         end if
        end do

         dk(:)=(x2(:)-x1(:))/ngrid(:)  !steps in k-space
         nvk=0
         do i=0,ngrid(1)
           fs(1)=real(i)
           do j=0,ngrid(2)
             fs(2)=real(j)
            do k=0,ngrid(3)
              fs(3)=real(k)
               nvk=nvk+1
               vk(:,nvk)=x1(:)+fs(:)*dk(:)       !k-vector components
            end do
           end do
         end do

        write(unit=*,fmt="( a,i6)") " =>  Total number of k_vectors :", nvk
        write(unit=lun,fmt="(/,a)")       " => k-vectors generated in the region:   "
        write(unit=lun,fmt="( a,2f8.4,a,i5,a)") "    Kx :",x1(1),x2(1), " with",ngrid(1)+1," points"
        write(unit=lun,fmt="( a,2f8.4,a,i5,a)") "    Ky :",x1(2),x2(2), " with",ngrid(2)+1," points"
        write(unit=lun,fmt="( a,2f8.4,a,i5,a)") "    Kz :",x1(3),x2(3), " with",ngrid(3)+1," points"
        write(unit=lun,fmt="( a,i6   )") "    Total number of k-vectors:",nvk

      Case default
          nvk=1
          vk(:,1)=0.0
      End Select
     ! if(iprint == 1) then
     !   open(unit=i_exc1,file=infil(1:ln)//".val",status="replace",action="write")
     ! end if

    End Subroutine genk

 End Module J_k_exchange
  !----------------------------------------------------------------------------


 Program enermag
   !use Mod_fun
   use CFML_GlobalDeps, only: Err_CFML
   use CFML_Maths, only: diagonalize_sh
   use CFML_gSpaceGroups, only: SPG_Type, Set_gSpG_from_string, Write_SpaceGroup_Info
   use j_k_exchange
   use Super_Exchange
   Implicit none
   integer, PARAMETER :: i_exc=1, i_enm=2, i_mom=7, i_res=4
   real, dimension(3,nv)  :: vk
   integer, dimension(nv) :: nvect
   real, dimension(3) :: ktar, rktar, rk
   character(len=80), dimension(id ) ::  Jotas
   character(len=80)   ::  TITLE, cmdline
   character(len=1)    ::  ANS
   character(len=20)   ::  spg
   character(len=132)  ::  forma, line
   character (len=256) ::  resfil,infil,outfil
   character (len=4)   ::  jota
   character (len=80)  ::  expo,name_Jota
   real, dimension(6)  :: ad
   LOGICAL :: ansf, compl, compl_tar
   COMPLEX, dimension(nat,nat) :: exchk,aop,eigen_cvectors
   COMPLEX, dimension(nat)  :: dtarc
   real, dimension(nat,nat) :: aux,eigen_rvectors
   real, dimension(nat)     :: eigen_val,dtarr
   real, dimension(ijo)     :: rang1,rang2
   real, dimension(ijo,njo) :: valjj
   integer, dimension(ijo)  :: ivar,npoi
   integer :: ier,i,j,nex,ln,nt,jnn,inn,lr,njjj,njotas,m,jj,ik,iopt, &
              iprint,inum,iphase,nojvar,ishifp,nvk,iex,n,nop,noptar, &
              iwrt, k, narg
   real    :: dmax,exch,emin,energy,rvalj,energytar,cpu_fin, cpu_ini,cpu_par, &
              cpu, remaining
   nex=0
   ansf=.false.
   ln=0
   lr=0
   narg=command_argument_count()
   if(narg /= 0) then
     call get_command_argument(1,infil)
     i=index(infil,".exc")
     if(i /= 0) infil=infil(1:i-1)
     ln=len_trim(infil)
     outfil=infil
     lr=ln
   end if
   if(narg > 1) then
     call get_command_argument(2,outfil)
     lr=len_trim(outfil)
   end if

   write(unit=*,fmt="(a)")"   *****************************************************"
   write(unit=*,fmt="(a)")"   ****               PROGRAM ENERMAG               ****"
   write(unit=*,fmt="(a)")"   *****************************************************"
   write(unit=*,fmt="(a)")"  "
   write(unit=*,fmt="(a)")"                 *** Version 1.5 *** "
   write(unit=*,fmt="(a)")"   *****************************************************"
   write(unit=*,fmt="(a)")"   * Calculates the Magnetic Energy for k-vectors in BZ*"
   write(unit=*,fmt="(a)")"   *      Uses a Classical Heisenberg Hamiltonian      *"
   write(unit=*,fmt="(a)")"   *        and isotropic exchange interactions        *"
   write(unit=*,fmt="(a)")"   *****************************************************"
   write(unit=*,fmt="(a)") "                   (JRC August-2001, LLB)"
   write(unit=*,fmt="(a)") " "
   write(unit=*,fmt="(a)") " "
   write(unit=*,fmt="(a)") " "
   write(unit=*,fmt="(a)") " "

   if(lr == 0) then
     write(unit=*,fmt="(a)",advance="no") " => Code of the file xx.exc (give xx): "
     read(unit=*,fmt="(a)") infil
     write(unit=*,fmt="(a,a,a)",advance="no") " => Code of the .out file ( <cr>= ",trim(infil),") :"
     read(unit=*,fmt="(a)") outfil
     lr=len_trim(outfil)
     IF(lr == 0) outfil=infil
   end if

   open(unit=i_exc,file=trim(infil)//".exc",status="old",action="read",position="rewind",iostat=ier)
   if(ier /= 0) then
     write(unit=*,fmt="(a)") " => File "//trim(infil)//".exc not found !!!"
     stop
   end if
   open(unit=i_enm,file=trim(outfil)//".enm",status="replace",action="write")
   open(unit=i_mom,file=trim(outfil)//".mom",status="replace",action="write")
   write(unit=*,fmt="(a)",advance="no") " => Do you want to save data in *.res file (y/n)?: "
   read(unit=*,fmt="(a)") ans
   IF(ans == "y" .OR. ans == "Y") THEN
     write(unit=*,fmt="(a,a,a)",advance="no") " => Code of the .res file ( <cr>= ",trim(outfil),") :"
     read(unit=*,fmt="(a)") resfil
     lr=len_trim(resfil)
     IF(lr == 0) resfil=outfil
     ansf=.true.
     open(unit=i_res,file=trim(resfil)//".res",status="replace",action="write")
     write(unit=*,fmt="(a)",advance="no")" => Give a comment for the .res file: "
     read(unit=*,fmt="(a)") title
     write(unit=i_res,fmt="(a)") title
   END IF

   ! Read input file infil.exc


   !     Call read_exchange(i_exc)

   write(unit=i_enm,fmt="(a)")"   *****************************************************"
   write(unit=i_enm,fmt="(a)")"   ****               PROGRAM ENERMAG               ****"
   write(unit=i_enm,fmt="(a)")"   *****************************************************"
   write(unit=i_enm,fmt="(a)")"  "
   write(unit=i_enm,fmt="(a)")"                   *** Version 2.0 *** "
   write(unit=i_enm,fmt="(a)")"   *****************************************************"
   write(unit=i_enm,fmt="(a)")"   * Calculates the Magnetic Energy for k-vectors in BZ*"
   write(unit=i_enm,fmt="(a)")"   *      Uses a Classical Heisenberg Hamiltonian      *"
   write(unit=i_enm,fmt="(a)")"   *        and isotropic exchange interactions        *"
   write(unit=i_enm,fmt="(a)")"   *****************************************************"
   write(unit=i_enm,fmt="(a)") "                  (JRC June-2023, ILL)"
   write(unit=i_enm,fmt="(a)") " "
   write(unit=i_enm,fmt="(a)") " "
   write(unit=i_enm,fmt="(a)") " "

   read(unit=i_exc,fmt="(a)") title
   write(unit=*,fmt="(a)") title
   IF(ansf) write(unit=i_res,fmt="(a)") title
   write(unit=i_enm,fmt="(/,a,/)") title
   write(unit=i_mom,fmt="(a)")    "      OUTPUT OF THE SPIN CONFIGURATIONS"
   write(unit=i_mom,fmt="(/,a,/)")  " => Warning!, this output is provisional!"
   write(unit=i_mom,fmt="(a,a,/)") " => Title:",title
   read(unit=i_exc,fmt="(i4,f8.4)") natcel,dmax
   IF(ansf) write(unit=i_res,fmt="(i5,a)") natcel, "   <--- Number of atoms"
   write(unit=i_enm,fmt="(a,i3)") " => Number of atoms: ",natcel
   write(unit=*,fmt="(a,i3)") " => Number of atoms: ",natcel
   write(unit=i_enm,fmt="(a,f8.3)") " => Maximum interatomic distance: ",dmax
   write(unit=*,fmt="(a,f8.3)") " => Maximum interatomic distance: ",dmax
   read(unit=i_exc,fmt="(a)") spg
   write(unit=i_enm,fmt="(a,a)") " => Space group: ",spg
   write(unit=*,fmt="(a,a)") " => Space group: ",spg

   call Set_gSpG_from_string("SPGR "//trim(spg),Grp)
   if(Err_CFML%Ierr /= 0) then
     write(unit=*,fmt="(a)") " => "//trim(Err_CFML%Msg)
     stop
   end if
   call Write_SpaceGroup_Info(Grp,i_enm)

   read(unit=i_exc,fmt="(3f8.4,3f8.3)") (ad(j),j=1,6)
   write(unit=i_enm,fmt="(a,/,a,3f8.4,3f8.3)") "     ",   &
       " => Unit cell parameters (Angstroms & degrees):",(ad(j),j=1,6)
   write(unit=*,fmt="(a,/,a,3f8.4,3f8.3)") "     ", &
       " => Unit cell parameters (Angstroms & degrees):",(ad(j),j=1,6)
   write(unit=i_enm,fmt="(/,a,/)") " => Atoms in the unit cell"
   write(unit=*,fmt="(/,a,/)") " => Atoms in the unit cell"
   write(unit=i_enm,fmt="(a,/)") "  No. Name      x        y        z       Spin"
   write(unit=*,fmt="(a,/)") "  No. Name      x        y        z       Spin"
   DO i=1,natcel
     read(unit=i_exc,fmt="(a6,tr1,4f9.5)") rnam(i),(xyz(j,i),j=1,3),s(i)
     IF(s(i) == 0.0) s(i)=1.0
     write(unit=i_enm,fmt="(i4,a,a6,4f9.5)")i,"  ",rnam(i),(xyz(j,i),j=1,3),s(i)
     write(unit=*,   fmt="(i4,a,a6,4f9.5)")i,"  ",rnam(i),(xyz(j,i),j=1,3),s(i)
   END DO
   write(unit=i_enm,fmt="(/,a)") "--------------------------------------------"
   write(unit=i_enm,fmt="(  a)") " => Input elements for constructing J_k(i,j)"
   write(unit=*,   fmt="(  a)") " => Input elements for constructing J_k(i,j)"
   write(unit=i_enm,fmt="(a,/)") "--------------------------------------------"
   DO i=1,natcel
     DO j=1,natcel
       read(unit=i_exc,fmt="(2i4,i5)") inn,jnn,nterm(i,j)
       IF(inn /= i .AND. jnn /= j)  &
           write(unit=*,fmt="(a)") " => Warning!, check the input exchange matrix!"
       write(unit=i_enm,fmt="(a,3(i2,a))") " => J(",i,",",j,")[K]   (",nterm(i,j), " terms)"
       write(unit=*,fmt="(a,3(i2,a))") " => J(",i,",",j,")[K]   (",nterm(i,j), " terms)"
       IF(nterm(i,j) /= 0) THEN
         DO nt=1,nterm(i,j)
           read(unit=i_exc,fmt="(a)")  line
           k=index(line,"J")
           if(k == 0) then
            write(unit=*,fmt="(a)") " => Error reading J in exchange file "
            stop
           end if
           read(unit=line(1:k-1),fmt=*) (trans(m,i,j,nt),m=1,3),exch
           jota=line(k:k+3)
           read(unit=line(k+1:k+3),fmt=*) nvalj(i,j,nt)
           name_jota=line(k+4:)
           !read(unit=i_exc,fmt="(t5,3f9.5,f10.3,t45,2a)")  &
           !    (trans(m,i,j,nt),m=1,3),exch,jota,name_jota
           CALL get_expo(trans(:,i,j,nt),expo)
           write(unit=i_enm,fmt="(a,a,3f6.2,a,f8.2,a,a,a)") "  ", &
               " Rn=(", (trans(m,i,j,nt),m=1,3)," ) J= ",exch, " --> ",jota,expo
           write(unit=*,fmt="(a,a,3f6.2,a,f8.2,a,a,a)") "  ", &
               " Rn=(", (trans(m,i,j,nt),m=1,3)," ) J= ",exch, " --> ",jota,expo
           IF(nvalj(i,j,nt) > nex) nex=nvalj(i,j,nt)
           valj(nvalj(i,j,nt))=exch
           jotas(nvalj(i,j,nt))=name_jota
         END DO
       END IF
     END DO
   END DO
   ! End reading input file

     write(unit=*,fmt="(a)") " => Input file read!!!"
   ! For small number of exchange parameters select a strategy
   ! of changing them for constructing a phase diagram
   DO
     write(unit=i_enm,fmt="(/,a)") "------------------------------------"
     write(unit=i_enm,fmt="(  a)") " => Input set of exchange parameters"
     write(unit=i_enm,fmt="(a,/)") "------------------------------------"
     write(unit=*,fmt="(/,a)") "------------------------------------"
     write(unit=*,fmt="(  a)") " => Input set of exchange parameters"
     write(unit=*,fmt="(a,/)") "------------------------------------"
     IF(ansf) THEN
       write(unit=i_res,fmt="(/,a)") "------------------------------------"
       write(unit=i_res,fmt="(  a)") " => Input set of exchange parameters"
       write(unit=i_res,fmt="(a,/)") "------------------------------------"
     END IF
     IF(nex < 20) THEN
       DO j=1,nex
         write(unit=*,fmt="(a,i1,a,f8.3,a,a)") "  J",j," =",valj(j),"  ",trim(jotas(j))
         write(unit=i_enm,fmt="(a,i1,a,f8.3,a,a)") "  J",j," =",valj(j),"  ",trim(jotas(j))
         IF(ansf) write(unit=i_res,fmt="(a,i1,a,f8.3,a,a)") "  J",j," =",valj(j),"  ",trim(jotas(j))
       END DO
       iphase=0
       write(unit=*,fmt="(a)",advance="no") " => Do you want to calculate a phase diagram (y/n)?: "
       read(unit=*,fmt="(a)") ans
       IF(ans == "y" .or. ans == "Y") iphase=1
     ELSE
       write(unit=*,fmt="(a)",advance="no") " => Too many exchange interactions, phase diagram cannot be calculated "
       DO j=1,9
         write(unit=*,fmt="(a,i1,a,f8.3,a,a)") "  J",j," =",valj(j),"  ",trim(jotas(j))
         write(unit=i_enm,fmt="(a,i1,a,f8.3,a,a)") "  J",j," =",valj(j),"  ",trim(jotas(j))
         IF(ansf) write(unit=i_res,fmt="(a,i1,a,f8.3,a,a)") "  J",j," =",valj(j),"  ",trim(jotas(j))
       END DO
       DO j=10,nex
         write(unit=*,fmt="(a,i2,a,f8.3,a,a)") "  J",j,"=",valj(j),"  ",trim(jotas(j))
         write(unit=i_enm,fmt="(a,i2,a,f8.3,a,a)") "  J",j,"=",valj(j),"  ",trim(jotas(j))
         IF(ansf) write(unit=i_res,fmt="(a,i2,a,f8.3,a,a)") "  J",j,"=",valj(j),"  ",trim(jotas(j))
       END DO
       iphase=0
     END IF

     ! Ask for changing J"s if no phase diagram is generated

     IF(iphase == 0) THEN
       write(unit=*,fmt="(a)",advance="no") " => Change exchange paramenters (y/n)?: "
       read(unit=*,fmt="(a)") ans
       IF(ans == "y".OR.ans == "Y") THEN
         DO j=1,id
           write(unit=*,fmt="(a)",advance="no") " -> Enter number of J and value (0 0: stop): "
           read(unit=*,fmt=*) inum, rvalj
           if(inum == 0) exit
           valj(inum)=rvalj
         END DO
         cycle
       END IF
     END IF
     IF(iphase == 1) THEN
       write(unit=*,fmt="(/,a,/)")" => Phase diagram will be calculated "
       write(unit=i_enm,fmt="(/,a,/)")" => Phase diagram will be calculated "
       write(unit=*,fmt="(a)",advance="no")" => Enter a target k-vector (3 reals): "
       read(unit=*,fmt=*) (ktar(i),i=1,3)
       DO i=1,ijo
         rang1(i)=0
         rang2(i)=0
         npoi(i)=1
       END DO
       do
         write(unit=*,fmt="(a,i2,a)",advance="no") " => Number of J's TO BE VARIED (<=",ijo,"): "
         read(unit=*,fmt=*) nojvar
         if(nojvar <= ijo) exit
         write(unit=*,fmt="(a)") " => Too many J's TO BE VARIED !"
       end do
       do
         njjj=1
         DO i=1,nojvar
           write(unit=*,fmt="(a,i1,a)",advance="no")  &
               " -> Number & Range of varied J-parameter ",i," & No of points: "
           read(unit=*,fmt=*) ivar(i),rang1(i),rang2(i),npoi(i)
           njjj=njjj*npoi(i)
         END DO
         IF(njjj > njo) THEN
           write(unit=*,fmt="(a)")  &
               " => Too many J-parameters! Reduce the number of points or increase the value of 'njo' in J_k_exchange module!"
           write(unit=*,fmt="(2(a,i7))")  &
               " => The number of requested J-points is: ",njjj,", the present maximum is: ",njo
           cycle
         END IF
         exit
       end do
       CALL genj(nojvar,njotas,rang1,rang2,npoi,valjj)
     ELSE
       njotas=1
       nojvar=0
     END IF
     IF(ansf) THEN
       write(unit=i_res,fmt="(i7,a)")  njotas,"   <--- Number of sets of J "
       DO i=1,nojvar
         write(unit=i_res,fmt="(a,i1,a, i2,2f10.4,i6)")  &
             " -> Number & Range of varied J-parameter ",i," & No of points: ",  &
             ivar(i),rang1(i),rang2(i),npoi(i)
       END DO
     END IF
     ishifp=njotas/24
     if(ishifp == 0) ishifp = 1
     IF(iphase == 1) then
         write(unit=*,fmt="(a,i7,a)") " => Phase diagram for: ",  &
         njotas," sets of exchange integrals"
         write(unit=i_enm,fmt="(a,i7,a)") " => Phase diagram for: ",  &
         njotas," sets of exchange integrals"
       DO i=1,nojvar
         write(unit=i_enm,fmt="(a,i1,a, i2,2f10.4,i6)")  &
             " -> Number & Range of varied J-parameter ",i," & No of points: ",  &
             ivar(i),rang1(i),rang2(i),npoi(i)
       END DO
     END IF
     iwrt=0
     !-----------------------------------------------------------------
     DO jj=1,njotas            !Start phase diagram
     !-----------------------------------------------------------------

       DO j=1,nojvar         !this loop is executed only if iphase > 0
         valj(ivar(j))=valjj(j,jj)
       END DO
       IF(iphase == 0) THEN
         write(unit=i_mom,fmt="(5(a,i1))") ("        J",j,j=1,nex)
         write(unit=i_mom,fmt="(5f10.2)") (valj(j),j=1,nex)
       END IF
       emin=9999999.0E+20
       !------------------------------------------
       ! Select strategy of searching of k-vectors
       !------------------------------------------
       IF(jj == 1) THEN
         CALL genk(i_enm,ln,infil,nvk,vk,iprint)
         Do ik=1,nvk
           nvect(ik)=0
         End Do
        IF(iphase == 1) THEN
         forma="(/,a, (a,i1),a,/)"
         write(unit=forma(6:6),fmt="(i1)") nojvar
         if(ansf) then
          write(unit=i_res,fmt=forma)"       Energy   Vector  Kx    Ky    Kz "  &
             , ("      J",ivar(j),j=1,nojvar),"                    Eigenvectors "
          write(unit=i_enm,fmt=forma)"       Energy   Targetk Kx    Ky    Kz "  &
             , ("      J",ivar(j),j=1,nojvar),"                    Eigenvectors "
         END IF
        END IF
        call cpu_time(cpu_ini)
        cpu=cpu_ini
       END IF
       IF(iphase == 0) THEN
         write(unit=i_enm,fmt="(/,a)")"-------------------------------------------------"
         write(unit=i_enm,fmt="(  a)")" => Calculation of energy for different k-vectors"
         write(unit=i_enm,fmt="(a,/)")"-------------------------------------------------"
         IF(ansf) write(unit=i_res,fmt="(i6,a)") nvk, "   <--- Number of k-vectors "
         iprint=1
       END IF
       !-------------------
       DO ik=1,nvk    !Loop over k-vectors
       !-------------------
       !---------------------------
       ! Construct the matrix J(k)
       !---------------------------
         rk(:)=vk(:,ik)
         CALL j_k(rk,exchk,iex)
         !----------------------------------------------------------
         ! Diagonalize exc, first check if imaginary component exist
         !----------------------------------------------------------
         n=natcel
         IF(iex == 0) THEN
           aux(1:natcel,1:natcel)=real(exchk(1:natcel,1:natcel))
           if(iprint == 0 ) then
             Call diagonalize_sh(aux,natcel,eigen_val)
           else
             Call diagonalize_sh(aux,natcel,eigen_val,eigen_rvectors)
           end if
         ELSE
           if(iprint == 0) then
             Call diagonalize_sh(exchk,natcel,eigen_val)
           else
             Call diagonalize_sh(exchk,natcel,eigen_val,eigen_cvectors)
           end if
         END IF

         IF(ansf .AND. iphase == 0) THEN
           forma="(3f8.4, a,  f14.2)"
           write(unit=forma(11:12),fmt="(i2)") natcel
           write(unit=i_res,fmt=forma)(rk(i),i=1,3)," ",(eigen_val(i),i=1,natcel)
         END IF

         IF(eigen_val(natcel) < emin ) THEN
           IF(iphase == 0) THEN
             write(unit=*,fmt="(a,3f8.4,a)") " => Eigenvalues for k = (",(rk(i),i=1,3)," )"
             write(unit=*,fmt="(99f12.3)") eigen_val(1:natcel)
             write(unit=i_enm,fmt="(a,3f8.4,a)") " => Eigenvalues & Eigenvectors for k = (",(rk(i),i=1,3)," )"
           !  write(unit=i_exc1,fmt="(a,3f8.4,a)") " => Eigenvalues & Eigenvectors for k = (",(rk(i),i=1,3)," )"
             forma="(f14.4,a,  f12.4,a)"
             j=natcel
             if(iex /= 0) j=2*j
             if(j < 10) then
               write(unit=forma(11:11),fmt="(i1)") j
             else
               write(unit=forma(10:11),fmt="(i2)") j
             end if
             if(iex /= 0) then
              do i=1,natcel
                write(unit=i_enm,fmt=forma ) eigen_val(i)," : (",eigen_cvectors(1:natcel,i),")"
              !  write(unit=i_exc1,fmt=forma ) eigen_val(i)," : (",eigen_cvectors(1:natcel,i),")"
              end do
             else
              do i=1,natcel
                write(unit=i_enm,fmt=forma ) eigen_val(i)," : (",eigen_rvectors(1:natcel,i),")"
              !  write(unit=i_exc1,fmt=forma ) eigen_val(i)," : (",eigen_rvectors(1:natcel,i),")"
              end do
             end if
           END IF
           IF(eigen_val(natcel) < emin) THEN     !Store the eigenvalue and eigenvectors
             iopt=ik                             !corresponding to the minimum energy
             emin=eigen_val(natcel)              !Update emin
             nop=natcel
             compl=.false.
             if(iex /= 0) then
               nop=nop*2
               compl=.true.
             end if
             aop(1:natcel,1:natcel)=exchk(1:natcel,1:natcel)
           END IF
         END IF
       !-------------------
       END DO    !End Loop over k-vectors ik=1,nvk
       !-------------------
       !------------------------------------------------------------
       ! Calculation of the spin configuration for the optimum energy
       !------------------------------------------------------------
       IF(compl) THEN
         Call diagonalize_sh(aop,natcel,eigen_val,eigen_cvectors)
       ELSE
         aux(1:natcel,1:natcel)=real(aop(1:natcel,1:natcel))
         Call diagonalize_sh(aux,natcel,eigen_val,eigen_rvectors)
       END IF

       IF(iphase == 0) THEN   !write only if no phase diagram is calculated

         write(unit=i_enm,fmt="(/,a)") "--------------------------------------------------------"
         write(unit=i_enm,fmt="(  a)") " => Exchange interaction matrix for the optimum k-vector"
         write(unit=i_enm,fmt="(a,/)") "--------------------------------------------------------"
         if(compl) then
           forma="( a,  (i15,tr5))"
         else
           forma="( a,  i10)"
         end if
         write(unit=forma(5:6),fmt="(i2)") natcel
         write(unit=i_enm,fmt=forma) "     ",(i,i=1,natcel)
         forma="(i3, a,  f10.2)"
         write(unit=forma(8:9),fmt="(i2)") nop
         if(compl) then
           DO i=1,natcel
             write(unit=i_enm,fmt=forma)i,"  ",(aop(i,j),j=1,natcel)
           END DO
         else
           DO i=1,natcel
             write(unit=i_enm,fmt=forma)i,"  ",(real(aop(i,j)),j=1,natcel)
           END DO
         end if
         write(unit=i_enm,fmt="(/,a)") "-------------------------------------------------------"
         write(unit=i_enm,fmt="(  a)") " => Eigenvalues and eigenvector of the optimum k-vector"
         write(unit=i_enm,fmt="(a,/)") "-------------------------------------------------------"
         write(unit=i_enm,fmt="(a,3f8.4,a)") " => Optimum k-vector -> (",(vk(i,iopt),i=1,3)," )"
         forma="(a,i2,a,  f10.4,a)"
         write(unit=forma(9:10),fmt="(i2)") nop
         if(compl) then
            DO i=1,natcel
              write(unit=i_enm,fmt="(a,i2,a,f14.4)")  "    Eigenvalue  E(k,",i,") = ",eigen_val(i)
              write(unit=i_enm,fmt=forma) "    Eigenvector V(k,",i,") = (",(eigen_cvectors(j,i),j=1,natcel),")"
            END DO
         else
            DO i=1,natcel
              write(unit=i_enm,fmt="(a,i2,a,f14.4)")  "    Eigenvalue  E(k,",i,") = ",eigen_val(i)
              write(unit=i_enm,fmt=forma) "    Eigenvector V(k,",i,") = (",(eigen_rvectors(j,i),j=1,natcel),")"
            END DO
         end if
       ELSE
       !------------- Below is for a phase diagram to be calculated
         IF(.NOT. ansf) THEN
           write(unit=i_enm,fmt="(5(a,i1))") ("        J",j,j=1,nex)
           write(unit=i_enm,fmt="(5f10.2)")     (valj(j),j=1,nex)
           write(unit=i_enm,fmt="(a,3f8.4,a)")  &
               " => Optimum k-vector -> (",(vk(i,iopt),i=1,3)," )"
           write(unit=i_enm,fmt="(a,f14.4)")  "    Emin = ",eigen_val(nop)
           forma="(a,i2,a,  f10.4,a)"
           write(unit=forma(9:10),fmt="(i2)") nop
           if(compl) then
               write(unit=i_enm,fmt=forma)  &
               "    Eigenvector V(k,",natcel,") = (",(eigen_cvectors(j,natcel),j=1,natcel),")"
           else
               write(unit=i_enm,fmt=forma)  &
               "    Eigenvector V(k,",natcel,") = (",(eigen_rvectors(j,natcel),j=1,natcel),")"
           end if
         !-------------
         ELSE
           forma="(f14.2,   i6, a,3f6.2, a,  f8.2,   a,  f10.4,a)"
           write(unit=forma(26:27),fmt="(i2)") nojvar
           write(unit=forma(38:39),fmt="(i2)") nop
           if(compl) then
               write(unit=i_res,fmt=forma) eigen_val(nop),iopt," ",(vk(i,iopt),i=1,3),"  ",(valj(ivar(j)),j=1,nojvar),  &
               "  (",(sign(1.0,real(eigen_cvectors(1,natcel)))*eigen_cvectors(j,natcel),j=1,natcel),")"
           else
               write(unit=i_res,fmt=forma) eigen_val(nop),iopt," ",(vk(i,iopt),i=1,3),"  ",(valj(ivar(j)),j=1,nojvar),  &
               "  (",(sign(1.0,eigen_rvectors(1,natcel))*eigen_rvectors(j,natcel),j=1,natcel),")"
           end if
           nvect(iopt)=nvect(iopt)+1
           IF(MOD(jj,ishifp) == 0) THEN
             iwrt=iwrt+1
             call cpu_time(cpu_par)
             cpu= (cpu_par-cpu)/60.0
             remaining=(24-iwrt)*cpu/60.0
             cmdline(1:80)=" "
             write(unit=cmdline(1:10),fmt="(i3,a)") int(remaining)," hours,"
             remaining=(remaining-int(remaining))*60.0
             write(unit=cmdline(12:21),fmt="(i2,a)") int(remaining)," minutes"
             forma="(a,i6,   i6, a,3f6.2,  f8.2)"
             write(unit=forma(22:23),fmt="(i2)") nojvar
             write(unit=*,fmt=forma) " -> Point #:",jj,iopt," ",(vk(i,iopt),i=1,3),  &
                                 (valj(ivar(j)),j=1,nojvar)
             write(unit=*,fmt="(a,f8.2,a,a,/)")"    Partial time:",cpu, &
             " minutes  ->  Approx. remaining time: ",trim(cmdline)
             cpu=cpu_par
           END IF
           ! Test for target vector
           IF(sum(ABS(ktar(:)-vk(:,iopt))) < 0.02) THEN
            !       12345678901234567890123456789012345678901234567890
            forma="(f14.4,   i6, a,3f6.2, a,  f8.2,   a,  f10.4,a)"
            write(unit=forma(26:27),fmt="(i2)") nojvar
            write(unit=forma(38:39),fmt="(i2)") nop
            if(compl) then     !complex

             if (real(eigen_cvectors(1,nop))  < 0.0 ) then
              write(unit=i_enm,fmt=forma) eigen_val(natcel),iopt," ",(vk(i,iopt),i=1,3),"  ",  &
                (valj(ivar(j)),j=1,nojvar),"  (",(-eigen_cvectors(j,natcel),j=1,natcel),")"
            !  forma="(f14.4,a,a,"//trim(forma(38:))
            ! write(*,*) " forma: ", forma
            ! do k=natcel-1,1,-1
            !   write(unit=i_enm,fmt=forma) eigen_val(k),"                                                   ",  &
            !   "  (",(-eigen_cvectors(j,k),j=1,natcel),")"
            ! end do
             else
              write(unit=i_enm,fmt=forma) eigen_val(natcel),iopt," ",(vk(i,iopt),i=1,3),"  ",  &
                (valj(ivar(j)),j=1,nojvar),"  (",(eigen_cvectors(j,natcel),j=1,natcel),")"
            ! forma="(f14.4,a,a,"//trim(forma(38:))
            ! write(*,*) " forma: ", forma
            ! do k=natcel-1,1,-1
            !   write(unit=i_enm,fmt=forma) eigen_val(k),"                                                   ",  &
            !   "  (",(eigen_cvectors(j,k),j=1,natcel),")"
            ! end do
             end if

            else ! no complex

             if (eigen_rvectors(1,nop)  < 0.0 ) then
              write(unit=i_enm,fmt=forma) eigen_val(natcel),iopt," ",(vk(i,iopt),i=1,3),"  ",  &
                (valj(ivar(j)),j=1,nojvar),"  (",(-eigen_rvectors(j,natcel),j=1,natcel),")"
            ! forma="(f14.4,a,a,"//trim(forma(38:))
            ! write(*,*) " forma: ", forma
            ! do k=natcel-1,1,-1
            !   write(unit=i_enm,fmt=forma) eigen_val(k),"                                                   ",  &
            !   "  (",(-eigen_cvectors(j,k),j=1,natcel),")"
            ! end do
             else
              write(unit=i_enm,fmt=forma) eigen_val(natcel),iopt," ",(vk(i,iopt),i=1,3),"  ",  &
                (valj(ivar(j)),j=1,nojvar),"  (",(eigen_rvectors(j,natcel),j=1,natcel),")"
            ! forma="(f14.4,a,a,"//trim(forma(38:))
            ! write(*,*) " forma: ", forma
            ! do k=natcel-1,1,-1
            !   write(unit=i_enm,fmt=forma) eigen_val(k),"                                                   ",  &
            !   "  (",(eigen_cvectors(j,k),j=1,natcel),")"
            ! end do
             end if

            end if  !complex
            rktar(:)=vk(:,iopt)
            compl_tar=compl
            if(compl_tar) then
              dtarc(1:natcel)=eigen_cvectors(1:natcel,natcel)
            else
              dtarr(1:natcel)=eigen_rvectors(1:natcel,natcel)
            end if
            energytar= eigen_val(natcel)
            noptar=nop
           END IF
         END IF
       END IF         !End of write only if no phase diagr...
       IF(iphase == 0) THEN
         rk(:)=vk(:,iopt)
         energy=eigen_val(natcel)
         if(compl) then
           CALL spin_conf(i_mom,rk,energy,eigen_cvectors(1:natcel,natcel))
         else
           CALL spin_conf(i_mom,rk,energy,eigen_rvectors(1:natcel,natcel))
         end if
       END IF
     !---------------------------------
     END DO              !njotas
     !---------------------------------
     IF(iphase == 1 .and. ansf) THEN

         if(compl) then
           CALL spin_conf(i_mom,rktar,energytar,dtarc)
         else
           CALL spin_conf(i_mom,rktar,energytar,dtarr)
         end if
       write(unit=i_enm,fmt="(a)") " => Frequency of optimum k-vectors"
       DO ik=1,nv
         IF(nvect(ik) > 1) THEN
           write(unit=i_enm,fmt="(a,3f6.2,a,i6,a,i7,a)")  &
               " k = (",(vk(j,ik),j=1,3),")   #:",ik," -> ",nvect(ik), " times"
         END IF
       END DO
     END IF
     call cpu_time(cpu_fin)
     !---- CPU times ----!
     write(unit=*,fmt="(/,/,t8,a,f8.2,a)") "Cpu Time: ", (cpu_fin - cpu_ini)/60.0, " min."
     write(unit=i_enm,fmt="(/,/,t8,a,f8.2,a)") "Cpu Time: ", (cpu_fin - cpu_ini)/60.0, " min."
     write(unit=*,fmt="(/,a)",advance="no")" => Do you want to continue (y/n)?: "
     read(unit=*,fmt="(a)") ans
     IF(ans == "y".OR.ans == "Y") cycle
     exit
   END DO
   STOP
 End Program EnerMag
