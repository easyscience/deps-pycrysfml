CELL 6.0 6.0 3.0 90 90 90

GENR x,y,z
BOX 0 1 0 1 0 12
ATOM Ce  Ce    0.0 0.0 0.0
ATOM Ni  Ni    0.5 0.5 0.5

{
K    0.0   0.0   0.123
K    0.0   0.0   0.000
LATTICE P
!
SYMM x,y,z
MSYM u,v,w,0.0
!
MATOM Ce   Ce  0.0 0.0 0.0  
skp   1 1  3.0 0.0 0.0   0.0 3.0 0.0  0.0
skp   2 1  0.0 0.0 2.0   0.0 0.0 0.0  0.0
MATOM Ni   Ni  0.5 0.5 0.5 
skp   1 1  3.0 3.0 0.0   0.0 0.0 0.0  0.0
}
