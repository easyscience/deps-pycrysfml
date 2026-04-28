class FortranVar():

    def __init__(self,name : str,ftype : str,kind : str='',ndim : int=0,dim : list=[],intent : str='',info : str='',len : str='',allocatable : bool=False, is_class : bool=False,value = None):

        self.name        = name
        self.ftype       = ftype
        self.kind        = kind
        self.ndim        = ndim
        self.dim         = dim
        self.intent      = intent
        self.info        = info
        self.len         = len
        self.allocatable = allocatable
        self.is_class    = is_class
        self.value       = value

class Subroutine():

    def __init__(self,name : str,module : str ='',arguments : dict ={},is_overload : bool = False,overload : str = '',has_interface : bool =False):

        self.name          = name
        self.module        = module
        self.arguments     = arguments.copy()
        self.is_overload   = is_overload
        self.overload      = overload
        self.has_interface = has_interface

class Function(Subroutine):

    def __init__(self,name : str,module : str ='',arguments : dict ={},xreturn : FortranVar = FortranVar('','')):

        super().__init__(name,module=module,arguments=arguments)
        self.xreturn = xreturn

class Interface():

    def __init__(self,name : str,procedures : list =[]):

        self.name       = name
        self.procedures = procedures.copy()

class FortranType():

    def __init__(self,name : str ='',parent : str ='',components : dict ={}):

        self.name       = name
        self.parent     = parent
        self.components = components.copy()

class Module():

    def __init__(self,name : str ='',types : dict ={},procedures : dict ={},publics : list =[],interface : dict ={}):

        self.name       = name
        self.types      = types.copy()
        self.procedures = procedures.copy()
        self.publics    = publics.copy()
        self.interface  = interface.copy()
