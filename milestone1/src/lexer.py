import lex
import re
import sys
import os

# -----------------------------------------------------------------------------
# lexer.py
#
# A simple calculator with variables -- all in one file.
# -----------------------------------------------------------------------------

errors = []
tokens = [ 'IDENTIFIER', 'LINECOMMENT', 'MULTILINECOMMENT', 'SPACES']

separators = {
    'LPAREN',
    'RPAREN',
    'LBRACES',
    'RBRACES',
    'LBRACKETS',
    'RBRACKETS',
    'SEMICOLON',
    'COMMA',
    'DOT',
    'ELLIPSIS',
    'AT',
    'DOUBLECOLON'
    }

operators = {
    'EQUAL',
    'GREATERTHAN',
    'LESSTHAN',
    'TYPE_ARG_BEGIN',
    'BOOLEANNOT',
    'TILDA',
    'QUESTIONMARK',
    'COLON',
    'ARROW',
    'EQUALS',
    'GREATERTHANEQUAL',
    'LESSTHANEQUAL',
    'NOTEQUALS',
    'AND',
    'OR',
    'PLUSPLUS',
    'MINUSMINUS',
    'PLUS',
    'MINUS',
    'MULTIPLY',
    'DIVIDE',
    'BOOLEANAND',
    'BOOLEANOR',
    'BOOLEANXOR',
    'MODULO',
    'LEFTSHIFT',
    'RIGHTSHIFT',
    'URIGHTSHIFT',
    'PLUSEQUALS',
    'MINUSEQUALS',
    'MULTIPLYEQUALS',
    'DIVIDEEQUALS',
    'ANDEQUALS',
    'OREQUALS',
    'XOREQUALS',
    'MODULOEQUALS',
    'LEFTSHIFTEQUALS',
    'RIGHTSHIFTEQUALS',
    'URIGHTSHIFTEQUALS'
    }

LITERALS = {
    'DECIMALINT',
    'HEXINT',
    'OCTALINT',
    'BINARYINT',
    'DECIMALFLOATINGLIT',
    'HEXFLOATINGLIT',
    'BOOLEANLIT',
    'CHARLIT',
    'STRINGLIT',
    'NULLLIT',
    }

tokens += separators
tokens += operators
tokens += LITERALS

################# Separators begin #################

t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACES = r'\{'
t_RBRACES = r'\}'
t_LBRACKETS = r'\['
t_RBRACKETS = r'\]'
t_SEMICOLON = r';'
t_COMMA = r','
t_DOT = r'\.'
t_ELLIPSIS = r'\.\.\.'
t_AT = r'@'
t_DOUBLECOLON = r'::'

###################################################


################# Operators begin #################

t_EQUAL = r'='
t_GREATERTHAN = r'>'
t_LEFTSHIFTEQUALS = r'<<='
t_LEFTSHIFT = r'<<'
t_LESSTHANEQUAL = r'<='

#  def t_TYPE_ARG_BEGIN(t):
    #  r''
    #  pass

t_BOOLEANNOT = r'!'
t_TILDA = r'~'
t_QUESTIONMARK = r'\?'
t_COLON = r':'
t_ARROW = r'->'
t_EQUALS = r'=='
t_GREATERTHANEQUAL = r'>='
t_NOTEQUALS = r'!='
t_AND = r'&&'
t_OR = r'\|\|'
t_PLUSPLUS = r'\+\+'
t_MINUSMINUS = r'\-\-'
t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_BOOLEANAND = r'\&'
t_BOOLEANOR = r'\|'
t_BOOLEANXOR = r'\^'
t_MODULO = r'%'
t_RIGHTSHIFT = r'>>'
t_URIGHTSHIFT = r'>>>'
t_PLUSEQUALS = r'\+='
t_MINUSEQUALS = r'-='
t_MULTIPLYEQUALS = r'\*='
t_DIVIDEEQUALS = r'\/='
t_ANDEQUALS = r'&='
t_OREQUALS = r'\|='
t_XOREQUALS = r'\^='
t_MODULOEQUALS = r'%='
t_RIGHTSHIFTEQUALS = r'>>='
t_URIGHTSHIFTEQUALS = r'>>>='

type_args_allowed = ['IDENTIFIER', 'COMMA', 'QUESTIONMARK', 'SUPER', 'EXTENDS', 'LBRACKETS', 'RBRACKETS', 'DOT']
def t_LESSTHAN(t):
    r'(?<!<)<(?!<)(?!=)'
    #  print(t.lexer.lexmatch)

    curr_pos = t.lexer.lexpos
    angular_brackets_begin_count = 1

    while (angular_brackets_begin_count != 0):
        tok = lexer.token()
        #  print("****Inside LESSTHAN: ", tok)
        if (tok.type in type_args_allowed):
            continue
        elif (tok.type == 'TYPE_ARG_BEGIN'):
            angular_brackets_begin_count += 1
        elif (tok.type == 'GREATERTHAN'):
            angular_brackets_begin_count -= 1
        else:
            break

    if (angular_brackets_begin_count == 0):
        t.type = 'TYPE_ARG_BEGIN'

    t.lexer.lexpos = curr_pos
    #  print()
    return t

###################################################

keywords = {
        'abstract': 'ABSTRACT',
        'continue': 'CONTINUE',
        'for': 'FOR',
        'new': 'NEW',
        'switch': 'SWITCH',
        'assert': 'ASSERT',
        'default': 'DEFAULT',
        'if': 'IF',
        'package': 'PACKAGE',
        'synchronized': 'SYNCHRONIZED',
        'boolean': 'BOOLEAN',
        'do': 'DO',
        'goto': 'GOTO',
        'private': 'PRIVATE',
        'this': 'THIS',
        'break': 'BREAK',
        'double': 'DOUBLE',
        'implements': 'IMPLEMENTS',
        'protected': 'PROTECTED',
        'throw': 'THROW',
        'byte': 'BYTE',
        'else': 'ELSE',
        'import': 'IMPORT',
        'public': 'PUBLIC',
        'throws': 'THROWS',
        'case': 'CASE',
        'enum': 'ENUM',
        'instanceof': 'INSTANCEOF',
        'return': 'RETURN',
        'transient': 'TRANSIENT',
        'catch': 'CATCH',
        'extends': 'EXTENDS',
        'int': 'INT',
        'short': 'SHORT',
        'try': 'TRY',
        'char': 'CHAR',
        'final': 'FINAL',
        'interface': 'INTERFACE',
        'static': 'STATIC',
        'void': 'VOID',
        'class': 'CLASS',
        'finally': 'FINALLY',
        'long': 'LONG',
        'strictfp': 'STRICTFP',
        'volatile': 'VOLATILE',
        'const': 'CONST',
        'float': 'FLOAT',
        'native': 'NATIVE',
        'super': 'SUPER',
        'while': 'WHILE'
}

################# Literals begin #################

def t_OCTALINT(t):
    r'0(([0-7]|_)*[0-7]+)*[0-7][lL]?'
    return t

def t_BINARYINT(t):
    r'0[bB]([0-1](([0-1]|_)*[0-1]+)*)[lL]?'
    return t

def t_HEXINT(t):
    r'0[Xx]([0-9a-fA-F](([0-9a-fA-F]|_)*[0-9a-fA-F]+)*)[lL]?'
    return t

def t_DECIMALFLOATINGLIT(t):
    r'(?<!_)((((\d|_)+(?<!_)\.(?!_)))|(\.(?!_)(\d|_)+))(\d|_)*(?<!_)[eE]?[\+-]?(\d|_)*(?<!_)[fFdD]?'
    return t

def t_DECIMALINT(t):
    r'(0|([1-9]((\d|_)*\d+)*))[lL]?'
    return t

def t_HEXFLOATINGLIT(t):
    r'0[xX]([0-9a-fA-F]|_)*(?<!_)[.]?(?!_)([0-9a-fA-F]|_)*(?<!_)[pP][\+-]?\d+[fFdF]?'
    return t

def t_BOOLEANLIT(t):
    r'(true|false)'
    return t

def t_CHARLIT(t):
    r'\'([^\'\\]|((\\[btnfr\"\'\\])+))\''
    return t

def t_STRINGLIT(t):
    r'(?<!\w)\"([^\"\\\n\r]|((\\[btnfr\"\'\\])+))*\"'
    return t

def t_NULLLIT(t):
    r'(null)'
    return t

###################################################

tokens = tokens + list(keywords.values())

def t_SPACES(t):
    r'(\ |\t)+'
    pass

def t_MULTILINECOMMENT(t):
    r'(?s)/\*.*?\*/'
    t.lexer.lineno += t.value.count("\n")
    pass

def t_LINECOMMENT(t):
    r'//[^\r\n]*'
    pass

def t_newline(t):
    r'[\n|\r]'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    line = t.lexer.lineno
    global errors
    errors += [(t.value[0], line)]
    t.lexer.skip(1)

def t_IDENTIFIER(t):
     r'(?<![0-9`\$\#])[a-zA-Z_][a-zA-Z_0-9]*'
     t.type = keywords.get(t.value, 'IDENTIFIER')    # Check for reserved words
     return t

#  lexer = lex.lex(debug=1)
lexer = lex.lex()

if (len(sys.argv) != 2):
    #  print("This script takes two parameters - first should be the java code file and the second should be the output file to output csv to. EXITING.")
    print("This script takes one single file as a parameter which should be a java code file. EXITING.")
    sys.exit(-1)

file_path = sys.argv[1]
#  csv_file = sys.argv[2] + ".csv"

if (not os.path.isfile(file_path)):
    print("The file doesn't exist. EXITING.")
    sys.exit(-1)

file1 = open(file_path)
program = file1.read()

lexer.input(program)

all_tokens = {}
while True:
    tok = lexer.token()
    if not tok:
        break

    tok_type = tok.type

    dict_key = (tok.value, tok_type)
    if dict_key in all_tokens:
        all_tokens[dict_key] += 1
    else:
        all_tokens[dict_key] = 1

#  print("%s\t\t%s\t\t%s" % ("Lexeme", "Token Type", "Count"))
#  print("-----------------------------------------------------")
#  for tok in all_tokens.keys():
    #  print("%s\t\t%s\t\t%d" % (tok[0], tok[1], all_tokens[tok]))

#  csv_arr = []
#  csv_arr += [["Lexeme", "Token Type", "Count"]]
#  for tok in all_tokens.keys():
    #  csv_arr += [[tok[0], tok[1], all_tokens[tok]]]

print("\n")
for i in errors:
    print("[Error]: Character %s not recognized on line %d" % (i[0], i[1]), file=sys.stderr)
