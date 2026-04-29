program main

    implicit none
    
    integer, parameter :: natoms = 4,nr=80
    integer :: i,j,k,n,m,u,v,w
    integer :: narg,gtype,ftype,nval,ndim,nasym
    integer, dimension(3) :: ngrid
    integer, dimension(4) :: version
    real, parameter :: dr = 0.10
    real :: pi,dist,vcell,a1,b1,c1,pol_tot,corr
    real, dimension(3) :: cell,ang,a,b,c,x,y,z,xx,yy,zz,r,t
    real, dimension(0:nr) :: rint
    real, dimension(3,natoms) :: xatom,xatom_red
    real, dimension(0:nr,natoms) :: integration,items
    real, dimension(:,:,:), allocatable :: cnt
    character(len=1) :: end_line=char(10)
    character(len=79) :: title
    character(len=256) :: filnam
    character(len=4), dimension(natoms) :: xname
    
    narg = Command_Argument_Count()
    if (narg > 0) then
        call Get_Command_Argument(1,filnam)
    else
        write(unit=*,fmt="(a)") &
            'Error! A pgrid file must be given on the command line'
        stop
    end if
    
    ! Read pgrid file
    open(11,file=filnam,form='unformatted', &
        access='stream',action='read')
    read(11) version
    read(11) title(1:79),end_line
    read(11) gtype
    read(11) ftype !=0                                                                                                             
    read(11) nval  !=1                                                                                                             
    read(11) ndim  !=3                                                                                                             
    read(11) ngrid
    read(11) nasym !                                                                                                               
    read(11) cell,ang  
    allocate(cnt(ngrid(1),ngrid(2),ngrid(3)))
    read(11) cnt  
    close(11)
    
    !open(19,file='scan.dat')
    !do i = 1 , ngrid(3)
    !    write(19,'(i6,f12.6)') i,cnt(i,33,66)
    !end do
    !close(19)
    !stop
    !Test reading
    write(*,'(a,4i6)') 'Version: ',version(:)
    write(*,'(a,i6)')  'Gtype:   ',gtype
    write(*,'(a,i6)')  'Ftype:   ',ftype
    write(*,'(a,i6)')  'Nval:    ',nval
    write(*,'(a,i6)')  'NDim:    ',ndim
    write(*,'(a,3i6)') 'NGrid:   ',ngrid(:)
    write(*,'(a,i12)')  'NAsym:   ',nasym
    write(*,'(a,3f12.6)') 'Cell:    ',cell(:)
    write(*,'(a,3f12.6)') 'Ang:     ',ang(:)
    
    ! Atom list
    xatom_red(:,1) = (/ 0.34066, 0.00733, 0.083333 /)
    xatom_red(:,2) = (/ 0.00000, 0.00000, 0.000000 /)
    xatom_red(:,3) = (/ 0.66667, 0.33333, 0.169378 /)    
    xatom_red(:,4) = (/ 0.50000, 0.50000, 0.500000 /)
    xname(1) = 'Fe1'
    xname(2) = 'Fe2'
    xname(3) = 'Fe3'
        
    ! Build unit cell
    pi     = acos(-1.0d0)
    a(1)   = cell(1)
    a(2:3) = 0.0
    b(1)   = cell(2) * cos(ang(3)*pi/180.)
    b(2)   = cell(2) * sin(ang(3)*pi/180.)
    b(3)   = 0.0
    c(1:2) = 0.0
    c(3)   = cell(3)
    a1 = (cell(1) / ngrid(1))
    b1 = (cell(2) / ngrid(2))
    c1 = (cell(3) / ngrid(3))
    vcell  = sqrt(3.) * a1 * b1 * c1 * 0.5
    pol_tot = sum(cnt)*vcell
    corr = (18.00 - pol_tot) / ngrid(1) / ngrid(2) / ngrid(3) / vcell 
        
    write(*,'(a,f12.6)') 'Vcell:   ',vcell
    write(*,'(a,f12.6)') 'Pol_Tot: ',pol_tot
    write(*,'(a,f12.6)') 'Corr:    ',corr
    
    ! Atomic coordinates
    do n = 1 , natoms   
        xatom(:,n) = xatom_red(1,n) * a(:) + &
            xatom_red(2,n) * b(:) + &
            xatom_red(3,n) * c(:) 
    end do
    
    ! Integration variables
    integration(:,:) = 0.0
    items(:,:) = 0
    do i = 1 , nr
        rint(i) = i * dr
    end do
    
    do i = 1 , ngrid(1)
        do j = 1 , ngrid(2)
            do k = 1 , ngrid(3)
                !if (cnt(i,j,k) < 0.3) cnt(i,j,k) = 0.0
            end do
        end do
    end do
    
    ! Integrate
    do n = 1 , natoms
        write(*,'(a,i2)') 'Integrating atom ',n
        do i = 0 , ngrid(1) - 1
            x(:) = (i + 0.5) * a(:) / ngrid(1)
            do j = 0 , ngrid(2) - 1
                y(:) = (j + 0.5) * b(:) / ngrid(2)
                do k = 0 , ngrid(3) - 1
                    z(:) = (k + 0.5) * c(:) / ngrid(3)
                    do u = -1 , 1
                        xx(:) = x(:) + u * a(:)
                        do v = -1 , 1
                            yy(:) = y(:) + v * b(:)
                            do w = - 1 , 1
                                zz(:) = z(:) + w * c(:)
                                r(:) = xx(:) + yy(:) + zz(:)
                                !r(:) = x(:) + y(:) + z(:)
                                t(:) = r(:) - xatom(:,n)
                                dist = sqrt(dot_product(t,t))
                                do m = 1 , nr
                                    if (dist < rint(m)) then
                                        integration(m,n) = &
                                         integration(m,n) + cnt(i+1,j+1,k+1)! + corr
                                        items(m,n) = items(m,n) + 1
                                    end if
                                end do
                            end do
                        end do
                    end do
                end do
            end do
        end do
    end do
    
    open(19, file = 'integration.dat', status = 'unknown')
    do i = 1 , nr
        write(19,'(f12.6)',advance='no') rint(i)
        do n = 1 , natoms
            write(19,'(f12.6)',advance='no') integration(i,n) * vcell!/ items(i,n)
        end do
        write(19,*)
    end do
    close(19)
        
        
    
end program