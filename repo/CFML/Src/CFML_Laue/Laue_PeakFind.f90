!!----
!!----
!!----
SubModule (CFML_Laue) Laue_PeakFind

    implicit none

    Contains

    Module Subroutine Peak_Find_Threshold(Fnd_par,Data2D,Mask_Excl,Mask2D,ExclR,Peaks)
        !> Subroutine Peak_Find_Threshold(Fnd_par, Data2D, Mask2D, ExclR,Peaks)
        !> Adapted from Snail (program peakfind of D19, refactoring of prewash by Mike Turner).
        !> Here the background is not uniform in the image, a division in blocks
        !> is controlled by the user and a more efficient detection of peaks is
        !> done. Also the inertia tensors are calculated with respect to the centre of mass.
        !> instead of using absolute pixels. The two half-axes of the inertia tensor
        !> and the cosine of the angle of the axis along with the maximum half-axis
        !> is found with the x-axis are stored in a vector component of Peaks: (a,b,cosAx).
        !>
        !> Created in August 2010 (JRC)
        !> Updated: January 2012  (JRC)

        ! Arguments
        type(PeakFind_Parameters_Type),         intent(in)  :: Fnd_par
        integer(kind=DATAKIND), dimension(:,:), intent(in)  :: Data2D     ! Converted data
        integer(kind=1), dimension(:,:),        intent(in)  :: Mask_Excl  ! Mask for excluded regions, set by Set_Mask_Excl
        integer(kind=1), dimension(:,:),        intent(out) :: Mask2D     ! Mask Data (0 = Background, 1 = maximum, 2 = nodal maximum,
                                                                          !            3 = peak area but not maximum)
        type(Excluded_Regions_Type),            intent(in)  :: ExclR
        type(Peak_List_Type),                   intent(out) :: Peaks
        !type(ProgressWindow),optional                      :: progress

        ! Local Variables 
        ! Integer,dimension(Size(Data2D,1),Size(Data2D,2)) :: DT
        ! Integer,Dimension(Size(Data2D,1))  :: jmax,jmin

        Integer,dimension(:,:), allocatable :: DT
        Integer,Dimension(:),   allocatable :: jmax,jmin

        Real(kind=cp),   Dimension(2)               :: cg,xpos, wpos
        Real(kind=cp),   Dimension(2,2)             :: tens,sigma_mat
        Real(kind=cp),   Dimension(3)               :: vtens, ellpar !Vectors containing the two semiaxes of the ellipse and
        Logical :: pass                             !the angle of the maximum semiaxis with the x-axis.
        Integer :: i,j,k,nprev,npks,imin,imax,n,m,npres,mpres,imov, &
                    jmov,mass,imass, isize, jsize, tmp, cnt, re, r2, &
                    j_min,j_max,netint, ii, jj, ie, je, jstep,maxv,minv,delta
        real(kind=cp)    :: bg,sigma, d2m, weight, ww, rmass, sxx, szz, sxz,det
        integer,      dimension(Size(Data2D,2)*(Size(Data2D,1)+Fnd_par%blocksz)/Fnd_par%blocksz/Fnd_par%blocksz+1) :: threshold, ibg
        real(kind=cp),dimension(Size(Data2D,2)*(Size(Data2D,1)+Fnd_par%blocksz)/Fnd_par%blocksz/Fnd_par%blocksz+1) ::   window
        type(Peak_List_Type) :: P_temp,P_aux
        integer, parameter   :: max_peaks=15000
        integer, dimension(max_peaks) :: poi, qoi
        integer, dimension(:), allocatable :: indx

        !if (present(progress)) call progress%ChangeSub("Preparation")
        !!  -> working around the commented lines that declares these variables
        allocate(DT(Size(Data2D,1),Size(Data2D,2)))
        allocate(jmax(Size(Data2D,1)))
        allocate(jmin(Size(Data2D,1)))

        allocate(P_temp%Peak(max_peaks))
        allocate(P_aux%Peak(max_peaks))
        Mask2D=0
        ! Mask pixels in excluded regions including the pixels with zero intensity
        where ( Data2D == 0)  Mask2D = -1
        where ( Mask_Excl == -1)  Mask2D = -1   !This supposes that the mask_Excl has been previously set
                                                !By calling the subroutine  Set_Mask_Excl
        isize = Size(Data2D,1)
        jsize = Size(Data2D,2)
        jstep=jsize/Fnd_par%blocksz
        ! open(unit=77,file="snail.deb",status="replace",action="write")
        ! write(77,"(a,4i5)")  "=> Size(Data2D,1), Size(Data2D,2), blocksz, jstep: ", isize, jsize, blocksz, jstep
        ! write(77,"(a)")    "   k   k-c    i    j       ibg       threshold"

            ! Calculation of the local background array and threshold
        k=0
        !if (present(progress)) call progress%ChangeSub("Local background calculation")
        do ie=1,isize-Fnd_par%blocksz+1,Fnd_par%blocksz
            do je=1,jsize-Fnd_par%blocksz+1,Fnd_par%blocksz
            k=k+1

            cnt=0; bg=0.0
            !  The commented lines below seem to create a problem in the algorithm (removed!)
            !maxv=maxval(Data2D(ie:ie+Fnd_par%blocksz-1,je:je+Fnd_par%blocksz-1))
            !minv=minval(Data2D(ie:ie+Fnd_par%blocksz-1,je:je+Fnd_par%blocksz-1))
            !delta=4*(maxv-minv)/5+minv
            do ii=0,Fnd_par%blocksz-1
                i=ie+ii
                do jj=0,Fnd_par%blocksz-1
                j=je+jj
                !if(Mask2D(i,j) /= -1 .and. Data2D(i,j) <= delta) then
                if(Mask2D(i,j) /= -1) then
                    cnt=cnt+1
                    bg=bg+Data2D(i,j)
                end if
                end do
            end do
            if(cnt > 0) then
                bg=bg/real(cnt,kind=cp)  !Average counts per pixel, taken as average local background
                sigma=sqrt(bg)   !Standard deviation of the local background
                ibg(k)=Nint(bg)
                ww=bg + Fnd_par%CutOff*sigma
                threshold(k)=Nint(ww)
                window(k)=Sqrt(ww)
            else
            ibg(k) = 0
            threshold(k)=0
            end if
            ! jj=((j-2)/Fnd_par%blocksz)+ ( (((i-2)/Fnd_par%blocksz)+1) -1)*jstep+1
            ! write(77,"(4i5,2i14)") k,jj,i,j,ibg(k),threshold(k)

            end do
            !if (present(progress)) call progress%update(int(30.0*ie/isize))
        end do
        !close(unit=77)

        nprev = 3
        npks = 0
        DT=Data2D

        !if (present(progress)) call progress%ChangeSub("Processing frames")
        global:  Do
                jmax = 0
                jmin = jsize+1 ! just has to be larger than all possible values of j
                pass = .FALSE.

            peak: Do i=nprev,isize-2 ! do whole frame except border
                Do j=3,jsize-2
                If (Mask2D(i,j) == -1) cycle
                k=(j/Fnd_par%blocksz)+ ( ((i/Fnd_par%blocksz)+1) -1)*jstep+1
                If (DT(i,j) > threshold(k)) Then  ! found new peak point
                    npks=npks+1                     ! increase number of peak points
                    npres=i                         ! save start square
                    mpres=j
                    imin=i                          ! row of start of peak
                    imax=i
                    imov=0                          ! going along + y
                    jmov=1                          ! "   "   "   "
                    pass=.true.
                    Exit peak
                End If
                End Do
            End Do peak
            !if (present(progress)) call progress%update(int(30.0+modulo(npks,60)))
            if(npks == max_peaks) then
            npks=npks-1
            write(unit=*,fmt="(a,i6,a,i3)") &
            "=> WARNING! maximum number of peaks reached! ",npks, " ... Increase the CutOff!, now at: ",Fnd_par%CutOff
            exit global
            end if
            If(.NOT. pass) Exit global            ! end of this frame (no peaks points found)

            Do
                k=(j/Fnd_par%blocksz)+ ( ((i/Fnd_par%blocksz)+1) -1)*jstep+1
                If (DT(i,j) > threshold(k)) Then
                If (jmax(i) < j) jmax(i)=j      ! record the end position for each row of peak
                If (jmin(i) > j) jmin(i)=j      ! record the start position for each row of peak
                If (imax < i) imax=i            ! record the vertical end position of the peak
                tmp = imov                      ! non-edge peak point, turn left
                imov= jmov
                jmov= -tmp
                Else
                tmp = imov
                imov= -jmov                     ! background,turn right
                jmov= tmp
                End If
                i=i+imov                          ! add move
                j=j+jmov
                If (i < 1 .or. j < 1 .or. i > isize .or. j > jsize) then
                !Write(unit=*,fmt="(a)") 'Warning: Trying to index outside Data2D'
                !Write(unit=*,fmt="(a)") 'Skiping this peak'
                npks=npks-1
                nprev=npres+1
                cycle global
                Endif

                If (i /= npres .or. j /= mpres) Cycle  ! Keep going if not back at the start of peak
                If (imov == -1 .or. jmov == 1) Exit    ! Did we walk in a loop?
                                                    ! Yes, then we have a reflection so exit
                                                    ! No, rogue pixel or horiz pixel line
            End Do

            ! Now munch it up
            nprev=npres
            mass=0                      ! Total points in contour
            imass=0
            rmass=0.0
            cg=0.0                      ! Initialise c. of g. (baricentre)
            tens=0.0                    ! Initialise inertia tensor
            wpos=0.0                    ! Intensity weighted cdg

            Do i=imin,imax
                xpos(1)=Real(i,kind=cp)                ! Load element to XPOS(N)
                Do j=jmin(i),jmax(i)
                k=(j/Fnd_par%blocksz)+ ( ((i/Fnd_par%blocksz)+1) -1)*jstep+1
                If (DT(i,j) > threshold(k)) Then   ! found peak point
                    xpos(2)=Real(j,kind=cp)              !
                    mass=mass+1
                    netint=DT(i,j)-Ibg(k)
                    weight=real(netint,kind=cp) !/window(k)
                    !weight=Max(0.0,Min(1.0,weight))
                    imass=imass+netint
                    rmass=rmass+weight
                    Do n=1,2
                    cg(n)=cg(n)+xpos(n)                    ! Form c. of g. sum
                    wpos(n)=wpos(n)+xpos(n)*weight         ! Form weighted c. of g. sum
                    End Do
                    DT(i,j) = 0                      ! remove peak point
                    Mask2D(i,j)=3                    ! Pixel in the area of a peak
                End If
                End Do
            End Do
            ! Discard peaks having less than Fnd_par%pk_ar pixels
            if(mass < Fnd_par%pk_ar) then
                Do i=imin,imax
                Do j=jmin(i),jmax(i)
                    if(Mask2D(i,j) == 3) Mask2D(i,j)=0  !Cleaning up the mask for discarded peaks
                End Do
                End Do
                npks=npks-1
                cycle global
            end if

            cg=cg/Real(mass,kind=cp)       ! Normalise (Baricentre)
            wpos=wpos/rmass        ! Normalise (Centroid)

            i=nint(wpos(1)); j= nint(wpos(2))
            if (i < 1 .or. j < 1 .or. i > isize .or. j > jsize) then
            write(unit=*,fmt="(a)") "  WARNING: indices of peak outside the frame!  ... peak discarded"
            npks=npks-1
            cycle
            else
            Mask2D(i,j) = 1  !Maximum of the peak (Centroid)
            end if

            ! Calculation of the Inertia Tensor (in fact the inverse of the covariance matrix)
            ! using weighted position
            !Now the calculation is done directly w.r.t. the c.d.g. calculated previously
            sxx=0.0; szz=0.0; sxz=0.0
            Do i=imin,imax
                xpos(1)=Real(i,kind=cp)
                Do j=jmin(i),jmax(i)
                k=(j/Fnd_par%blocksz)+ ( ((i/Fnd_par%blocksz)+1) -1)*jstep+1
                If (Data2D(i,j) > threshold(k)) Then   ! Use here the original data because DT is put to zero
                    xpos(2)=Real(j,kind=cp)              !
                    netint=Data2D(i,j)-Ibg(k)
                    weight=real(netint,kind=cp)/rmass    ! Normalise for getting directly the covariance matrix
                    !weight=Max(0.0,Min(1.0,weight))
                    !Change i,j to x,z by taking into account i->z,j->x
                    szz=szz+weight*(xpos(1)-wpos(1))**2
                    sxx=sxx+weight*(xpos(2)-wpos(2))**2
                    sxz=sxz+weight*(xpos(1)-wpos(1))*(xpos(2)-wpos(2))
                End If
                End Do
            End Do
            sigma_mat(1,1) = sxx
            sigma_mat(2,2) = szz
            sigma_mat(1,2) = sxz
            sigma_mat(2,1) = sigma_mat(1,2)
            call Sigma_Ellipse(sigma_mat,ellpar)
            !Tens is 1/2 of the inverse of the covariance matrix Sigma^(-1)(factor 2 in det)
            det=2.0*(sxx*szz-sxz*sxz)
            tens(1,1)= szz/det   !txx => ellipse  txx x^2 + tzz z^2+ 2txz xz = k
            tens(2,2)= sxx/det   !tzz
            tens(1,2)= -sxz/det  !txz
            tens(2,1)=tens(1,2)

            call Itens_Ellipse(tens,vtens)
            !write(*,"(2(a,3f10.2))") "ellpar =",ellpar,"   vtens =",vtens
            P_temp%Peak(npks)%pos=wpos
            P_temp%Peak(npks)%cdm=cg
            P_temp%Peak(npks)%Itens=vtens
            P_temp%Peak(npks)%npix=mass
            P_temp%Peak(npks)%Intensity=imass
            !----------------------------------------------------------
            !       back for more..
            !----------------------------------------------------------
        End Do global

        !if (present(progress)) call progress%update(90)
        !if (present(progress)) call progress%ChangeSub("Cleaning found peaks")
        if(npks == 0) then
            Peaks%N_peaks=npks
            return
        end if

        ! Treatment of peaks before creating the output list
        ! Remove peaks too close between them (conserving the most representatitive one)
        ! First order the peaks by position: close peaks are close in the list
        ! Eliminate also the peaks with zero or negative integrated intensity just after
        ! getting the pointers for ordering the peaks.
        do i=1,npks
            poi(i)=i
        end do
        call In_Sort(Nint(P_temp%Peak(:)%pos(1)),npks, poi, qoi )
        poi(1:npks)=qoi(1:npks)
        call In_Sort(Nint(P_temp%Peak(:)%pos(2)),npks, poi, qoi )
        poi(1:npks)=0

        do i=1,npks
            P_aux%Peak(i)=P_temp%Peak(qoi(i))
        end do
        P_temp=P_aux

        d2m=real(Fnd_par%d_min*Fnd_par%d_min,kind=cp)

        !Mark the peaks within excluded region that have appeared from the snail algorithm
        do i=1,npks
            ii=nint(P_temp%Peak(i)%pos(1)); jj= nint(P_temp%Peak(i)%pos(2))
            if(Mask_Excl(ii,jj) == -1) then
                poi(i)=1
                Mask2D(ii,jj)=0
            end if
        end do

        !Now mark the peaks with negative intensity
        do i=1,npks
            if(P_temp%Peak(i)%Intensity < 1.0) then
            ii=nint(P_temp%Peak(i)%pos(1)); jj= nint(P_temp%Peak(i)%pos(2))
            poi(i)=1
            Mask2D(ii,jj)=0
            end if
        end do

        !open(unit=77,file="snail.deb",status="replace",action="write")
        do i=1,npks-1
            ie=nint(P_temp%Peak(i)%pos(2))
            !write(77,"(i8,2f12.4)")  i,P_temp%Peak(i)%pos
            do j=i+1,npks
            je= nint(P_temp%Peak(j)%pos(2))
            if(je-ie > Fnd_par%d_min) exit
            wpos= P_temp%Peak(i)%pos - P_temp%Peak(j)%pos
            if (dot_product(wpos,wpos) < d2m ) then
                if( P_temp%Peak(i)%Intensity > P_temp%Peak(j)%Intensity) then
                    poi(j)=1
                    ii=nint(P_temp%Peak(j)%pos(1)); jj= nint(P_temp%Peak(j)%pos(2))
                else
                    poi(i)=1
                    ii=nint(P_temp%Peak(i)%pos(1)); jj= nint(P_temp%Peak(i)%pos(2))
                end if
                Mask2D(ii,jj)=0
            end if
            end do
        end do
        n= sum(poi(1:npks))
        !Now allocate the output list of peaks
        Peaks%N_peaks=npks-n
        if(allocated(Peaks%Peak)) deallocate(Peaks%Peak)
        allocate(Peaks%Peak(Peaks%N_peaks))
        n = 0
        do i=1,npks
            if(poi(i) == 1) cycle
            n=n+1
            Peaks%Peak(n)= P_temp%Peak(i)
            !write(unit=*,fmt="(i6,i4,2f12.4,f14.0)") n, Peaks%Peak(n)%npix,  Peaks%Peak(n)%pos,  Peaks%Peak(n)%Intensity
        end do
        n=Peaks%N_peaks

        !Now re-copy the Peaks into P_temp to sort them by increasing intensity
        deallocate(P_temp%Peak)
        allocate(P_temp%Peak(n),indx(n))
        P_temp%Peak(1:n)=Peaks%Peak(1:n)
        indx = Sort(P_temp%Peak(:)%Intensity,n)
        do i=1,n
            j=indx(i)
            Peaks%Peak(n-i+1)= P_temp%Peak(j)
        end do

        !if (present(progress)) call progress%update(100)

    End Subroutine Peak_Find_Threshold

    Module Subroutine Set_Mask_Excl(ExclR,DataM,Mask_Excl)
        !> This subroutine should be called before using peak search algorithms.
        !> The excluded region Mask_Excl matrix is allocated to be of the same size as
        !> the intent(in) image stored in DataM. It is used for eliminating visible 
        !> reflections from excluded regions in order
        !> to avoid wrong integrated intensities.

        ! Arguments
        type(Excluded_Regions_type),                              intent (in)   :: ExclR
        integer(kind=DATAKIND),      dimension(:,:),              intent (in)   :: DataM
        integer(kind=1),             dimension(:,:), allocatable, intent(inout) :: Mask_Excl

        ! Local variables
        integer :: i,j,n,i_min,i_max,j_min,j_max,r2,re, Nrow, Ncol
        real(kind=cp)    :: a, b, h, k, x, y

        Nrow=size(DataM,1); Ncol=size(DataM,2)
        if(allocated(Mask_Excl)) deallocate(Mask_Excl)
        allocate(Mask_Excl(Nrow,Ncol))
        !First check if there is a circular inverse Mask
        if(ExclR%inv_circ /= 0) then
            Mask_Excl=-1 ! all pixels are excluded, test now those that are allowed
            n=ExclR%inv_circ
            i_min=ExclR%exc_circ(1,n)-ExclR%exc_circ(3,n)
            i_max=ExclR%exc_circ(1,n)+ExclR%exc_circ(3,n)
            j_min=ExclR%exc_circ(2,n)-ExclR%exc_circ(3,n)
            j_max=ExclR%exc_circ(2,n)+ExclR%exc_circ(3,n)
            re= ExclR%exc_circ(3,n)*ExclR%exc_circ(3,n)
            i_min=max(1,i_min); j_min=max(1,j_min)
            i_max=min(Nrow,i_max); j_max=min(Ncol,j_max)
            do j=j_min,j_max
            do i=i_min,i_max
                r2= (i-ExclR%exc_circ(1,n))**2+(j-ExclR%exc_circ(2,n))**2
                if( r2 <= re) Mask_Excl(i,j) = 0
            end do
            end do
        else if(ExclR%inv_rect /= 0) then !Now check if there is a rectangular inverse Mask
            Mask_Excl=-1 ! all pixels are excluded, test now those that are allowed
            do j=ExclR%exc_rect(2,1,n), ExclR%exc_rect(2,2,n)
                do i=ExclR%exc_rect(1,1,n), ExclR%exc_rect(1,2,n)
                    Mask_Excl(i,j) = 0
                end do
            end do
        else
            Mask_Excl=0  !All pixels are allowed
        end if

        where ( DataM <= 1)  Mask_Excl = -1 !low intensity areas (zero or one counts)
        do n=1,ExclR%Nexc_Rect
            if(n == ExclR%inv_rect) cycle !skip inverse mask
            !if ( ExclR%elliptic(n)) then
            !    a = abs(ExclR%exc_rect(2,2,n) - ExclR%exc_rect(2,1,n))/2.0
            !    b = abs(ExclR%exc_rect(1,2,n) - ExclR%exc_rect(1,1,n))/2.0
            !    h=(ExclR%exc_rect(2,2,n) + ExclR%exc_rect(2,1,n))/2.0
            !    k=(ExclR%exc_rect(1,2,n) + ExclR%exc_rect(1,1,n))/2.0
            !    do j=ExclR%exc_rect(2,1,n), ExclR%exc_rect(2,2,n)
            !        do i=ExclR%exc_rect(1,1,n), ExclR%exc_rect(1,2,n)
            !            y=i
            !            x=j
            !            if(  (x-h)**2.0 /a**2  +  (y-k)**2.0 /b**2 < 1.0  )then
            !                Mask_Excl( i, j) = -1
            !            end if
            !        end do
            !    end do
            !else
                do j=ExclR%exc_rect(2,1,n), ExclR%exc_rect(2,2,n)
                    do i=ExclR%exc_rect(1,1,n), ExclR%exc_rect(1,2,n)
                        Mask_Excl(i,j) = -1
                    end do
                end do
            !end if
        end do

        do n=1,ExclR%Nexc_Circ
        if(n == ExclR%inv_circ) cycle !skip inverse mask
        i_min=ExclR%exc_circ(1,n)-ExclR%exc_circ(3,n)
        i_max=ExclR%exc_circ(1,n)+ExclR%exc_circ(3,n)
        j_min=ExclR%exc_circ(2,n)-ExclR%exc_circ(3,n)
        j_max=ExclR%exc_circ(2,n)+ExclR%exc_circ(3,n)
        re= ExclR%exc_circ(3,n)*ExclR%exc_circ(3,n)
        i_min=max(1,i_min); j_min=max(1,j_min)
        i_max=min(Nrow,i_max); j_max=min(Ncol,j_max)
        do j=j_min,j_max
            do i=i_min,i_max
                r2= (i-ExclR%exc_circ(1,n))**2+(j-ExclR%exc_circ(2,n))**2
                if( r2 <= re) Mask_Excl(i,j) = -1
            end do
            end do
        end do

    End Subroutine Set_Mask_Excl
    
    Subroutine In_Sort(id,n,p,q)
        !> Subroutine to order in ascending mode the integer array "id".
        !> The input value "n" is the number of items to be ordered in "id".
        !> The array "p" is the initial pointer to "id" (coming from a previous call)
        !> The final pointer holding the order of items.
        !
        !> This subroutine has been taken from the module CFML_Math_General of the old CrysFML library
        !> because it was used by Peak_Find_Threshold.
        !>
        !> Update: November - 2008
           
        ! Arguments 
        integer, dimension(:), intent(in) :: id  !Integer array to be sorted
        integer,               intent(in) :: n   !Number items in the array
        integer, dimension(:), intent(in) :: p   !Initial pointer from a previous related call
        integer, dimension(:), intent(out):: q   !Final pointer doing the sort of id

        ! Local Variables
        integer :: i,j,k,l,m
        integer, dimension(:),allocatable :: it

        l=minval(id)
        m=maxval(id)
        l=l-1
        m=m-l
        allocate(it(m))
        it(1:m)=0
        do i=1,n
            j=id(p(i))-l
            it(j)=it(j)+1
        end do
        j=0
        do i=1,m
            k=j
            j=j+it(i)
            it(i)=k
        end do
        do i=1,n
            j=id(p(i))-l
            it(j)=it(j)+1
            j=it(j)
            q(j)=p(i)
        end do

        return

    End Subroutine In_Sort

    Subroutine Itens_Ellipse(itens,vtens)  !itens= 1/2 Sigma^(-1) (variance-covariance matrix)

        ! Arguments
        real(kind=cp), dimension(2,2), intent(in ) :: itens
        real(kind=cp), dimension(3),   intent(out) :: vtens

        ! Local variables
        real(kind=cp) :: a,b,e,f, theta, epsl,epsm, area,s,c,s2,c2,s2t,a2,b2

        epsl=1.0E-8_cp; epsm=1.0E-4_cp
        if (abs(itens(2,2) - itens(1,1)) < epsl) then
            theta=0.0
            f=0.5*(itens(1,1)+itens(2,2))
            a=1.0/sqrt(f)
            vtens=(/a,a,0.0_cp/)
        else
            theta= -0.5*Atan2( 2.0*itens(1,2) , (itens(2,2)-itens(1,1)) )
            s=Sin(theta); c=Cos(theta)
            s2=s*s; c2=c*c; s2t=Sin(2.0*theta)
            e=itens(1,1)*c2+itens(2,2)*s2-S2t*itens(1,2) !1/a2
            f=itens(1,1)*s2+itens(2,2)*c2+S2t*itens(1,2) !1/b2
            a2=1.0/Max(e,epsm); b2=1.0/Max(f,epsm)
            b=Sqrt(b2); a=Sqrt(a2)
            if( a > b) then
            vtens(1)=a ; vtens(2)=b ; vtens(3)=theta*to_deg
            else
            vtens(1)=b ; vtens(2)=a ; vtens(3)=theta*to_deg + 90.0
            end if
        end If

    End Subroutine Itens_Ellipse

    Subroutine Sigma_Ellipse(sigma,ellPar)

        ! Arguments
        real(kind=cp), dimension(2,2), intent(in)  :: sigma
        real(kind=cp), dimension(3),   intent(out) :: ellPar !:(max-semiaxis,min-semiaxis,angle of max-semiaxis with the x-axis)
        
        ! Local variables
        real(kind=cp), dimension(2) :: lamb
        integer,       dimension(1) :: imax,imin
        real(kind=cp)               :: a,b,e,f,g,h,epsl,theta

        epsl=1.0E-8
        e=sigma(1,1)+sigma(2,2); f=sigma(1,1)-sigma(2,2); g=4.0*sigma(1,2)*sigma(1,2)
        h=sqrt(f*f+g)
        if(h < epsl) then  !Degenerate double solution
            a=sqrt(e)
            ellpar=(/a,a,0.0_cp/) !Circle
        else
            lamb=0.5*(/e-h, e+h/)  !True eigenvalues of sigma
            imax=maxloc(lamb)
            imin=minloc(lamb)
            if(abs(sigma(1,2)) < epsl) then !ellipse in canonical orientation
                select case(imax(1))
                    case(1)
                    theta=0.0
                    case(2)
                    theta=90.0
                end select
            else
                select case(imax(1))
                    case(1)
                    theta=atan2( lamb(1)-sigma(1,1), sigma(1,2) )*to_deg
                    case(2)
                    theta=atan2( lamb(2)-sigma(1,1), sigma(1,2) )*to_deg
                end select
            end if
            a=sqrt(2.0*lamb(imax(1))); b=sqrt(2.0*lamb(imin(1))) !the factor 2 comes from the fact that we consider
            ellpar=(/a,b,theta/)                                 !the ellipse corresponding to 1/2 the inverse of sigma
        end if

    End Subroutine Sigma_Ellipse

End Submodule Laue_PeakFind