from pygments.lexer import inherit,RegexLexer
from pygments.token import Comment,Keyword,Name,Number,Text

class CFLLexer(RegexLexer):

    name = "cfl"
    aliases = ["cfl"]

    tokens = {
        'root': [
            (r'^\s*!.*',Comment),
            (r'^\s*ATOM\b',Name.Builtin),
            (r'^\s*CELL\b',Name.Builtin),
            (r'^\s*GEN\b',Name.Builtin),
            (r'^\s*HALL\b',Name.Builtin),
            (r'^\s*KVEC\b',Name.Builtin),
            (r'^\s*KVECT\b',Name.Builtin),
            (r'^\s*MOMENT\b',Name.Builtin),
            (r'^\s*NKVEC\b',Name.Builtin),
            (r'^\s*NKVECT\b',Name.Builtin),
            (r'^\s*NQVEC\b',Name.Builtin),
            (r'^\s*NQVECT\b',Name.Builtin),
            (r'^\s*QVEC\b',Name.Builtin),
            (r'^\s*QVECT\b',Name.Builtin),
            (r'^\s*Q_COEFF\b',Name.Builtin),
            (r'^\s*SPACEG\b',Name.Builtin),
            (r'^\s*SPGR\b',Name.Builtin),
            (r'^\s*TITLE\b',Name.Builtin),
            (r'\s*[+-]?[0-9]+(\.[0-9]+)',Number),
            (r'[\w\W]',Text),
            inherit
        ]
    }
