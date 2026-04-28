from pygments.lexers.shell import BashLexer
from pygments.lexer import inherit
from pygments.token import Comment,Keyword,Name

class ExtendedBashLexer(BashLexer):

    name = "bash_extension"
    aliases = ["bash_extension"]

    tokens = {
        'root': [
            (r'\bcd\b',Name.Builtin),
            (r'\bcmake\b',Name.Builtin),
            (r'\bmake\b',Name.Builtin),
            (r'\bmkdir\b',Name.Builtin),
            (r'\bnmake\b',Name.Builtin),
            (r'\bpython\b',Name.Builtin),
            (r'\btar\b',Name.Builtin),
            (r'\binstall\b',Keyword),
            (r'--?[a-zA-Z0-9][\w-]*',Keyword),
            inherit
        ]
    }
