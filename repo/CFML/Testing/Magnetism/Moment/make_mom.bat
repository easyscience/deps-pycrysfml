   ifort /c moment.f90 /Ox /nologo /I. /I %CRYSFML%\ifort_release\include
   link /subsystem:console /stack:102400000 /out:moment.exe *.obj %CRYSFML%\ifort_release\lib\libCrysFML08.a
   del *.obj *.mod
