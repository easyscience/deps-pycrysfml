!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Locate
 implicit none
 Contains

    !!----
    !!---- LOCATE_I
    !!----    Locate the index j of an ordered array V(:)
    !!----    satisfying:
    !!----
    !!----          V(J) <= X < V(J+1)
    !!----
    !!----  If X is outside the range of the V(:) values j=0
    !!----
    !!---- 28/03/2019
    !!
    Pure Module Function Locate_I(V,x,n) Result(j)
       !---- Argument ----!
       integer, dimension(:), intent(in):: v  ! Input vector
       integer,               intent(in):: x  ! Value
       integer, optional,     intent(in):: n  ! Dimension
       integer                          :: j

       !---- Local Variables ----!
       integer :: jl, ju, jm, i1, i2, nt


       !> Limits
       i1=lbound(v,dim=1)
       i2=ubound(v,dim=1)
       if (present(n)) then
          nt=i2-i1+1
          if (nt > n) then
             i2=i1+n-1
          end if
       end if

       !> Check
       if (x <  v(i1) .or. x >  v(i2) ) then
          j=0
          return
       end if

       jl=i1-1
       ju=i2+1
       do
          if(ju-jl <= 1) exit
          jm=(ju+jl)/2
          if ((v(i2) > v(i1)) .eqv. (x > v(jm))) then
             jl=jm
          else
             ju=jm
          end if
       end do
       j=jl

    End Function Locate_I

    !!----
    !!---- LOCATE_R
    !!----    Locate the index j of an ordered array V(:)
    !!----    satisfying:
    !!----
    !!----               V(J) <= X < V(J+1)
    !!----
    !!---- Update: June - 2011
    !!
    Pure Module Function Locate_R(V,x,n) Result(j)
       !---- Argument ----!
       real(kind=cp), dimension(:), intent(in):: v
       real(kind=cp),               intent(in):: x
       integer, optional,           intent(in):: n
       integer                                :: j

       !---- Local Variables ----!
       integer :: jl, ju, jm, i1,i2, nt

       !> Limits
       i1=lbound(v,dim=1)
       i2=ubound(v,dim=1)
       if (present(n)) then
          nt=i2-i1+1
          if (nt > n) then
             i2=i1+n-1
          end if
       end if

       !> Init
       j=i1-1

       !> Check
       if (x <= v(i1)) then
          j=i1
          return
       end if

       if (x >= v(i2)) then
          j=i2
          return
       end if

       jl=i1-1
       ju=i2+1
       do
          if(ju-jl <= 1) exit
          jm=(ju+jl)/2
          if ((v(i2) > v(i1)) .eqv. (x > v(jm))) then
             jl=jm
          else
             ju=jm
          end if
       end do
       j=jl
    End Function Locate_R

End Submodule Maths_Locate
