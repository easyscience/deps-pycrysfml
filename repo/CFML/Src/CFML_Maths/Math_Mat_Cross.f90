!!----
!!---- SUBMODULE CFML_Math_3D
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Mat_Cross
 implicit none
 Contains

    !!----
    !!---- MAT_CROSS_C
    !!----    Calculates the matrix corresponding to the operator u x
    !!----    Antisymmetric matrix of the form:
    !!----                /  0   -u(3)  u(2)\
    !!----    M=[u]cross=|  u(3)   0   -u(1) |
    !!----                \-u(2)  u(1)   0  /
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Mat_Cross_C(Vec) Result(M)
       !---- Argument ----!
       complex(kind=cp), dimension(3), intent( in) :: Vec
       complex(kind=cp), dimension(3,3)            :: M

       M = reshape( (/  (0.0_cp,0.0_cp),   -vec(3),         vec(2),  &
                            vec(3),   (0.0_cp,0.0_cp),     -vec(1),  &
                           -vec(2),           vec(1),   (0.0_cp,0.0_cp)/),(/3,3/))
    End Function Mat_Cross_C

    !!----
    !!---- MAT_CROSS_R
    !!----    Calculates the matrix corresponding to the operator u x
    !!----    Antisymmetric matrix of the form:
    !!----                /  0   -u(3)  u(2)\
    !!----    M=[u]cross=|  u(3)   0   -u(1) |
    !!----                \-u(2)  u(1)   0  /
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Mat_Cross_R(Vec) Result(M)
       !---- Argument ----!
       real(kind=cp), dimension(3), intent( in) :: Vec
       real(kind=cp), dimension(3,3)            :: M

       M = reshape( (/ 0.0_cp,   -vec(3),     vec(2),  &
                        vec(3),    0.0_cp,   -vec(1),  &
                       -vec(2),     vec(1),    0.0_cp/),(/3,3/))
    End Function Mat_Cross_R

    !!----
    !!---- MAT_CROSS_I
    !!----    Calculates the matrix corresponding to the operator u x
    !!----    Antisymmetric matrix of the form:
    !!----                /  0   -u(3)  u(2)\
    !!----    M=[u]cross=|  u(3)   0   -u(1) |
    !!----                \-u(2)  u(1)   0  /
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Mat_Cross_I(Vec) Result(M)
       !---- Argument ----!
       integer, dimension(3), intent( in) :: Vec
       integer, dimension(3,3)            :: M

       M = reshape( (/   0,    -vec(3),    vec(2),  &
                        vec(3),    0,     -vec(1),  &
                       -vec(2),   vec(1),     0 /),(/3,3/))
    End Function Mat_Cross_I

End Submodule Maths_Mat_Cross
