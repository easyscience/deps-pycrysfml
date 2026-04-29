import os

def make_ifort_windows(modules : dict):

	OPT="/c /fpp /nologo /Warn /Qdiag-disable:10448"
	with open('../Python_API/scripts/windows/make_ifort.bat','w') as f:
		f.write(f"@echo off")
		f.write(f"\n\nset INSTALLATION_DIR=")
		f.write(f"\nset CRYSFML08_INSTALL=")
		f.write(f"\nset LIBPYTHON=")
		f.write(f"\nif not exist %INSTALLATION_DIR% (")
		f.write(f"\n    mkdir %INSTALLATION_DIR%")
		f.write(f"\n)")
		for m in modules:
			f.write(f"\n\nrem py_{m.lower()}")
			f.write(f"\necho Building py_{m.lower()}.pyd")
			f.write(f"\nifort {OPT} ..\\..\\py_{m.lower()}.f90 /I%CRYSFML08_INSTALL%\\include")
			f.write(f"\nlink py_{m.lower()}.obj /out:py_{m.lower()}.dll /libpath:%CRYSFML08_INSTALL%\\lib /dll %LIBPYTHON% CrysFML08.lib")
			f.write(f"\nmove py_{m.lower()}.dll %INSTALLATION_DIR%\\py_{m.lower()}.pyd")

def make_ifort_linux(modules : dict):

	pass

def scripts(modules : dict):

	if not os.path.isdir('../Python_API/scripts'):
		os.mkdir('../Python_API/scripts')
	if not os.path.isdir('../Python_API/scripts/windows'):
		os.mkdir('../Python_API/scripts/windows')
	if not os.path.isdir('../Python_API/scripts/linux'):
		os.mkdir('../Python_API/scripts/linux')
	make_ifort_windows(modules)
	make_ifort_linux(modules)

