   !!---- Program Phase_Diagram
   !!---- Author: J. Rodriguez-Carvajal (LLB,ILL,CSIC)
   !!----
   !!---- This console program helps to visualise a magnetic phase diagram as a function of
   !!---- exchange interactions using GFourier from the FullProf Suite. The program reads
   !!---  a file (with extension *.res) coming from Enermag (when used for generating a
   !!---  phase diagram). A maximum of three variable exchange interactions are taken into
   !!---  account. And generates some information in the screen as well as a binary file
   !!---  that can be read from GFourier
   !!---
   Program Phase_diagram
      use CFML_maths, only: zbelong
      use CFML_gSpaceGroups
      use CFML_Atoms
      use CFML_Metrics
      Use CFML_Strings, only: Frac_Trans_1DIG, Pack_String

      Implicit none
      integer, parameter  :: n_sk=96
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


      integer, parameter         :: nsites=4, numa=40, numag=500
      integer                    :: i, nj, ns, ier, nvec, nsub, magtype,maxl
      integer                    :: inum, i1,i2,i3, num_mag
      integer, dimension(nsites) :: nats
      integer, dimension(numag)  :: mag_types, freq_mag
      real, dimension(5)         :: valj
      real, dimension(5,numag)   :: jdomin
      real, dimension(5,numag)   :: jdomax
      character (len=35)         :: kvector
      real, dimension(3)         :: vk,tvk
      real, dimension(numa)      :: eigenvr
      character (len=132)        :: fileres,forma,fform
      character (len=1)          :: parenth, ans
      character (len=512)        :: line
      real                       :: energ,jmin,jmax
      logical                    :: esta
      integer,           dimension(nsites*numa) :: magt
      character(len=1),  dimension(nsites*numa) :: magc
      character(len=512),dimension(numag)       :: magchar

    ! Density information
   !---- Title or reference ----!
      character (len=80) :: titulo=' Magnetic Phase Diagram'

   !---- Symmetry parameters ----!
      character (len=20)            :: spgr               ! Space Group
      type (Cell_G_Type)            :: celda              ! Cell type structure
      class(SPG_Type), allocatable  :: grp_espacial       ! Space Group structure
      integer, dimension(3)         :: ngrid
      integer                       :: ntype=1,jfilbin=15,j,k
      REAL,    allocatable,dimension(:,:,:) :: rho
      integer, allocatable,dimension(:,:,:) :: freq
      real                  :: denmin=1.0,denmax
      real, dimension (2)   :: xlim=(/0.0,1.0/)   ! xmin,xmax
      real                  :: smin=0.0           ! Bound of Smin
      real                  :: smax=1.0           ! Bound of Smax
      real                  :: sigm=2.0           ! Bound of Sigma(Fo)
      real, dimension (2)   :: ylim=(/0.0,1.0/)   ! ymin,ymax
      real, dimension (2)   :: zlim=(/0.0,1.0/)   ! zmin,zmax
      real, dimension (3)   :: cel,celang
      type (atlist_type)    :: atomos
      spgr='P -1'                 !Artificial values, just for generating a pseudo-density map
      cel=(/5.0,5.0,5.0/)
      celang=(/90.0,90.0,90.0/)
      call Set_Crystal_Cell(cel,celang,celda)
      call Set_SpaceGroup(spgr,grp_espacial)
      call Allocate_Atom_List(2,atomos,"Atm_Type",0)
      atomos%atom(1)%lab='Fe'
      atomos%atom(1)%x=[0.5,0.5,0.5]
      atomos%atom(1)%occ=1.0
      atomos%atom(2)%lab='Co'
      atomos%atom(2)%x=[0.0,0.0,0.0]
      atomos%atom(2)%occ=1.0


      jdomin =  100000.0
      jdomax = -100000.0

      write(*,'(a)')  '   ------------------------------------------------------------------------ '
      write(*,'(a)')  '     Program Phase_Diagram: generates binary file for use with GFourier '
      write(*,'(a)')  '   It needs a *.res file from EnerMag when a Phase Diagram has been written '
      write(*,'(a/)') '   ------------------------------------------------------------------------ '
      write(*,'(a)',advance="no") ' => Code of the *.res file: '
      read(*,'(a)') fileres
      open(unit=1,file=trim(fileres)//'.res',status='old')
      open(unit=3,file=trim(fileres)//'.ana',status='unknown')

      write(*,'(a)',advance="no") ' => Number of Js (only the first 3 Js may be varied!): '
      read(*,*) nj
      write(*,'(a)',advance="no") ' => Number of Sites: '
      read(*,*) ns
      nsub=0
      do i=1,ns
       write(*,'(a,i2,a)',advance="no") ' => Number of atoms for site:',i,': '
       read(*,*) nats(i)
       nsub=nsub+nats(i)
      end do

       k=0
       do
         read(1,'(a)',iostat=ier) line
         if(ier /=0) then
           write(*,'(a)') " Item: 'points:' not found!! "
           stop
         end if
         i=index(line,'points:')
         if(i /= 0) then
           j=index(line,':')
           k=k+1
           read(line(j+1:),*) inum,jmin,jmax,ngrid(k)
           if(k == 3 .or. k == nj) exit
         end if
       end do

       do
         read(1,'(a)',iostat=ier) line
         if(ier /=0) then
           write(*,'(a)') " Item: 'Energy   Vector' not found!! "
           stop
         end if
         line=adjustl(line)
         if(line(1:15) == 'Energy   Vector') then
           read(1,'(a)') line
           exit
         end if
       end do

       if(ngrid(3) == 0) ngrid(3)=1

       denmax=-1.0
       num_mag=0
       freq_mag=0
       allocate (rho(ngrid(1),ngrid(2),ngrid(3)))
       allocate (freq(ngrid(1),ngrid(2),ngrid(3)))
       freq=0
       do i1=1,ngrid(1)
        do i2=1,ngrid(2)
         do i3=1,ngrid(3)

         read(1,'(a)', iostat=ier)  line
         if(ier /=0) exit
         read(line(1:21),*) energ,nvec
         forma=' '
         i=(index(line,")")-index(line,"("))/10
         if(i > nsub) then
                  !1234567890123456789012345!!8901234567!!012345
            FORMa='(f14.4,   i6,1x,3f6.2,2x,  f8.2,2x,a,  f10.4)'
            WRITE(FORMa(26:27),'(i2)') nj
            WRITE(FORMa(38:39),'(i2)') nsub*2
         else     !1234567890123456789012345!!8901234567!!012345
            FORMa='(f14.4,i6   ,1x,3f6.2,2x,  f8.2,2x,a,  f10.4)'
            WRITE(FORMa(26:27),'(i2)') nj
            WRITE(FORMa(38:39),'(i2)') nsub
         end if
            read(line,forma)  energ, nvec, vk(:), valj(1:nj),parenth,eigenvr(1:nsub) !,eigenvi(1:nsub)
            tvk=2.0*vk
         if(.not. zbelong(tvk)) then
           magtype = 0
           magt(:) = 0
           magc(:) = '0'
         else
           magtype=0
           do i=1,nsub
             if(eigenvr(i) > 0.0) then
               magt(i) = 1
               magc(i) = '+'
             else if(eigenvr(i) < 0.0) then
               magt(i) = 2
               magc(i) = '-'
             else
               magt(i) = 0
               magc(i) = '0'
             end if
             magtype = magtype + magt(i)*(2**(nsub-i))
           end do
           magtype = 2*nvec*(2**nsub) + magtype
         end if

     !Select the different magnetic structures
         if(num_mag == 0) then
           num_mag=num_mag+1
           mag_types(num_mag)   = magtype
           freq_mag(num_mag)=freq_mag(num_mag)+1
           jdomin(1:nj,num_mag) = valj(1:nj)
           jdomax(1:nj,num_mag) = valj(1:nj)
           write(magchar(num_mag),'(a,i2,a,110(a1,1x))') '(',nvec,': ',magc(1:nsub),')'
           kvector = Frac_Trans_1DIG(vk)
           magchar(num_mag) = trim(magchar(num_mag))//' k = '//kvector

           j= num_mag
           freq_mag(j)=freq_mag(j)+1
         else
           esta=.false.
           do i=1,num_mag
             if(magtype == mag_types(i)) then
               j=i
               esta=.true.
               freq_mag(j)=freq_mag(j)+1
             end if
           end do
           if(esta) then
             do i=1,nj
               if( valj(i)  < jdomin(i,j) )  jdomin(i,j) = valj(i)
               if( valj(i)  > jdomax(i,j) )  jdomax(i,j) = valj(i)
             end do
           else
             num_mag=num_mag+1
             mag_types(num_mag)   = magtype
             jdomin(1:nj,num_mag) = valj(1:nj)
             jdomax(1:nj,num_mag) = valj(1:nj)
             write(magchar(num_mag),'(a,i2,a,110(a1,1x))') '(',nvec,': ',magc(1:nsub),')'
             kvector=Frac_Trans_1DIG(vk)
             magchar(num_mag) = trim(magchar(num_mag))//' k = '//kvector
             j=num_mag
             freq_mag(j)=freq_mag(j)+1
           end if

         end if

         forma= '(  f8.2,2x,i10,5x,  i1,4x,3f8.4)'
         WRITE(FORMa(2:3),'(i2)') nj
         WRITE(FORMa(19:20),'(i2)') nsub
  !      write(2,form) valj(1:nj), j, magt(1:nsub), vk(:)
         freq(i1,i2,i3) = j
         if(j > denmax) denmax=j

         end do
        end do
       end do

       write(3,'(a,a/)')'  ANALYSIS OF MAGNETIC STRUCTURE TYPES IN: ',trim(fileres)//'.res'
       write(3,'(a,i3)') ' => Number of distinct magnetic structures: ',num_mag
             !1234567890123456789012345678901
       forma='(i5,2x,i12,2x,a,t60 ,  (3x,2f6.1),i10)'
       maxl=0
       do i=1,num_mag
         j=len_trim(magchar(i))
         if(j > maxl) maxl=j
       end do
       maxl=22+maxl
       write(forma(18:20),'(i3.3)') maxl
       write(forma(22:23),'(i2)') nj
       !      123456789012345678
       fform="(a,t   ,a,t   ,a /)"
       write(fform(5:7),'(i3.3)') maxl+10
       write(fform(12:14),'(i3.3)') maxl+15*nj
       write(3,'(a//)')  ' => List of magnetic structure types: '
       write(3,fform) ' Type        Code          Vk-Sign sequence','J-domains    ','      Frequency'
       write(*,'(a//)')  ' => List of magnetic structure types: '
       write(*,fform) ' Type        Code          Vk-Sign sequence','J-domains    ','      Frequency'

       do i=1,num_mag
         write(3,forma) i,mag_types(i),trim(magchar(i)), &
                            (jdomin(j,i),jdomax(j,i),j=1,nj), freq_mag(i)
         write(*,forma) i,mag_types(i),trim(magchar(i)), &
                            (jdomin(j,i),jdomax(j,i),j=1,nj), freq_mag(i)
       end do

       write(*,'(a)')              ' => Density = Log(Frequency) (f)          : '
       write(*,'(a)')              '    Density = Code           (c)          : '
       write(*,'(a)')              '    Density = Order          (o)          : '
       write(*,'(a)',advance="no") '    Density = User defined   (u) (<cr>=u)?: '
       read(*,'(a)') ans

       if( ans == 'o' .or. ans == 'O') then
         forall (i1=1:ngrid(1), i2=1:ngrid(2), i3=1:ngrid(3))
           rho(i1,i2,i3) =freq(i1,i2,i3)
         end forall
       else if( ans == 'f' .or. ans == 'F') then
         forall (i1=1:ngrid(1), i2=1:ngrid(2), i3=1:ngrid(3))
           rho(i1,i2,i3) =log(real(freq_mag(freq(i1,i2,i3))))
         end forall
       else if( ans == 'c' .or. ans == 'C') then
         forall (i1=1:ngrid(1), i2=1:ngrid(2), i3=1:ngrid(3))
           rho(i1,i2,i3) =  mag_types(freq(i1,i2,i3))
         end forall
       else
          write(*,'(/a/)')'                    Type  Vk-Sign seq.     ...    Frequency'
         do i=1,num_mag
             write(*,'(a,i3,2x,a,i8,a)',advance="no") ' => Density value for',i,trim(magchar(i)), freq_mag(i),': '
             read(*,*) freq_mag(i)
         end do
         forall (i1=1:ngrid(1), i2=1:ngrid(2), i3=1:ngrid(3))
           rho(i1,i2,i3) =freq_mag(freq(i1,i2,i3))
         end forall
       end if

       call crea_filebin()

       write(*,'(a,a)') ' => Binary file    : ',trim(fileres)//'.bin'
       write(*,'(a,a)') ' => Summary file   : ',trim(fileres)//'.ana'
       stop

    contains

   !!----------------------------------------------------------------!!
   !!---- SUBROUTINE: CREA_FILEBIN                               ----!!
   !!----             Creantes a binary file for the map         ----!!
   !!----------------------------------------------------------------!!
   subroutine crea_filebin()

      !---- Definition of variables ----!
      integer :: i,j,k
      !!! Complementary variables for Fourier compatibility
      character (len=150) :: fildat   ! Data file
      character (len=150) :: filbin   ! Fourier binary file
      character (len=150) :: filhkl   ! Reflections file
      character (len=150) :: filatm   ! Atom file
      character(len=5)    :: version= "04.06"
      integer,dimension (9) :: lugar  ! Codes for reading
      integer :: jlist=0              ! Control of directive for list information
      integer :: npeaks_to_find=0     ! Number of peaks to search
      real    :: dist1=2.0,dist2=2.0,dist3=3.0  ! distance between peaks
      real :: xinc                    ! Increment on X
      real :: yinc                    ! Increment on Y
      real :: zinc                    ! Increment on Z
      real :: denabsmax               ! Maximum Absolute Density
      real :: denave=5.0              ! Average Density
      real :: densigma=0.0            ! Standar deviation of the Density Map
      real :: f000 =0.0               ! Value of F000
      real :: f000s=0.0               ! Value of F000-Scale
      lugar(:)=0
      denabsmax=denmax
      fildat = ' '
      filbin = ' '
      filhkl = ' '
      filatm = ' '
       xinc= 1.0/real(ngrid(1))
       yinc= 1.0/real(ngrid(2))
       zinc= 1.0/real(ngrid(3))

      !---- Opening binary file  ----!
      open(jfilbin,file=trim(fileres)//'.bin',form='unformatted',access='stream',status='replace',action="write")

      !---- Writting the information ----!

      write (jfilbin) titulo
      write (jfilbin) version
      call Write_Bin_Crystal_Cell(celda,jfilbin)
      call Write_SpaceGroup_Bin(grp_espacial,jfilbin)
      call Write_Bin_Atom_List(Atomos,jfilbin)
      write (jfilbin) fildat,filbin,filhkl,filatm,lugar,jlist,dist1,dist2,dist3
      write (jfilbin) f000,f000s,ntype,smin,smax,sigm,npeaks_to_find
      write (jfilbin) ngrid,denmin,denmax,xlim,ylim,zlim,xinc,yinc,zinc
      write (jfilbin) denave,densigma

      do i=1,ngrid(1)
         do j=1,ngrid(2)
            write (jfilbin) (rho(i,j,k),k=1,ngrid(3))
         end do
      end do

      !---- closing the file ----!
      close (jfilbin)

      return
   end subroutine crea_filebin

   End Program Phase_diagram
