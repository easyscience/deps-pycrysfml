"""
Python script for building CFML_Wraps.
Author: Nebil A. Katcho
February 2024
"""

class FortranVar():
    """
    Class representing a Fortran variable.
    """
    def __init__(self,name : str,ftype : str,kind : str='',ndim : int=0,
        dim : list=[],intent : str='inout',info : str='',len : str='',
        allocatable : bool=False,optional :bool=False,is_class : bool=False,
        value = None,parent : str='',ptype : str=''):

        self.name        = name.strip()
        self.ftype       = ftype
        self.kind        = kind
        self.ndim        = ndim
        self.dim         = dim
        self.intent      = intent
        self.info        = info
        self.len         = len
        self.allocatable = allocatable
        self.optional    = optional
        self.parent      = parent
        self.is_class    = is_class
        self.value       = value
        self.ptype       = ptype

class Subroutine():

    def __init__(self,name : str,module : str ='',arguments : dict ={},
        warguments : list =[],is_overload : bool = False,overload : str = '',
        has_optionals : bool =False,has_interface : bool =False,
        docu : list=[],wreturn : list =[],inout : list=[],
        nwarg : int=0, nwman : int=0, nwopt : int=0):

        self.name          = name.strip()
        self.module        = module
        self.arguments     = arguments.copy()
        self.docu          = docu.copy()
        self.is_overload   = is_overload
        self.overload      = overload
        self.has_interface = has_interface
        self.has_optionals = has_optionals
        self.inout         = inout.copy() # variables with intent=inout
        self.warguments    = warguments.copy() # arguments of the wrapper
        self.wreturn       = wreturn.copy() # return of the wrapper
        self.nwarg          = nwarg # Number of total arguments of the wrapper
        self.nwman          = nwman # Number of mandatory arguments of the wrapper
        self.nwopt          = nwopt # Number of optional arguments of the wrapper

class Function(Subroutine):

    def __init__(self,name : str,module : str ='',arguments : dict ={},
        xreturn : FortranVar = FortranVar('','')):

        super().__init__(name,module=module,arguments=arguments)
        self.xreturn = xreturn

class Interface():

    def __init__(self,name : str,procedures : list =[]):

        self.name       = name.strip()
        self.procedures = procedures.copy()

class FortranType():

    def __init__(self,name : str ='',parent : str ='',childs : list=[],
        components : dict ={}):

        self.name       = name.strip()
        self.parent     = parent
        self.childs     = childs.copy()
        self.components = components.copy()

class Module():

    def __init__(self,name : str ='',uses : list=[],types : dict ={},
        procedures : dict ={},publics : list =[],interface : dict ={},
        wraps : list=[], unwraps : list=[]):

        self.name       = name.strip()
        self.types      = types.copy()
        self.procedures = procedures.copy()
        self.publics    = publics.copy()
        self.interface  = interface.copy()
        self.uses       = uses.copy()
        self.wraps      = wraps.copy()
        self.unwraps    = unwraps.copy()
