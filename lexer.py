#!/usr/bin/python
import sys
import lex

keywords = 	('abstract','continue','for','new','switch','assert','default','if','package','synchronized','boolean','do',   'goto','private' ,'this'
,'break','double','implements','protected','throw','byte','else','import','public','throws','case','enum','instanceof','return','transient','catch','extends','int','short','try','char','final','interface','static','void','class','finally','long','strictfp','volatile'
,'const','float','native','super','while')

operators= ('plus','minus','star','divide','and','or','eq','le','lt','ge','gt','ne','not','tilda','ques','colon','pointer','andand','oror','plusplus','minusminus'
	,'xor','percent','lshift','rshift','rrshift','peq','meq','seq','deq','andeq','oreq','xoreq','percenteq','rshifteq','lshifteq','rrshifteq')

separators=('lbrace','rbrace','lparen','rparen','lbracket','rbracket','semicolon','dot','coma','threedots','fourdots','at_the_rate')

liters =('hex','decimal','oct','string','bin','bool','null','ch','true','false')

tokens = ('lcomment','bcomment','id') + keywords + operators + separators + liters


# regular expressions for operators
t_plus= r'\+'
t_minus= r'-'
t_star= r'\*'
t_divide= r'/'
t_and= r'&'
t_or= r'\|'
t_eq= r'='
t_le= r'<='
t_lt= r'<'
t_ge= r'>='
t_gt= r'>'
t_ne= r'!='
t_not= r'!'
t_tilda= r'~'
t_ques= r'\?'
t_colon= r':'
t_pointer= r'->'
t_andand=r'&&'
t_oror= r'\|\|'
t_plusplus= r'\+\+'
t_minusminus= r'--'
t_xor= r'\^'
t_percent= r'%'
t_lshift= r'<<'
t_rshift= r'>>'
t_rrshift= r'>>>'
t_peq= r'\+='
t_meq= r'-='
t_seq= r'\*='
t_deq= r'/='
t_andeq= r'&='
t_oreq= r'\|='
t_xoreq= r'\^='
t_percenteq= r'%='
t_rshifteq= r'>>='
t_lshifteq= r'<<='
t_rrshifteq= r'>>>='


#regular expressions for separators
t_lparen= r'\('
t_rparen= r'\)'
t_lbrace= r'{'
t_rbrace= r'}'
t_lbracket= r'\['
t_rbracket= r'\]'
t_semicolon= r';'
t_dot= r'\.'
t_coma= r','
t_threedots= r'\.\.\.'
t_fourdots= r'::'
t_at_the_rate= r'@'

#regular expressions for comments
t_lcomment = r'//[^\r\n]*'
t_bcomment = r'/\*(.|\n)*\*/'

#regular expression for literals
t_decimal = r'((0|[1-9][0-9_]*)\.([0-9][0-9_]*)?([eE][+-]?[0-9][0-9_]*)?[fFdD]?|(0|[1-9][0-9_]*)([eE][+-]?[0-9][0-9_]*)?[fFdDlL]?|\.([0-9][0-9_]*)?([eE][+-]?[0-9][0-9_]*)?[fFdD]?)'
t_oct = r'0[_]*[0-7][0-7_]*[lL]?'
t_bin = r'0[bB]([01]|[01][01_]*[01])[lL]?'
t_bool = r'true|false'
t_null = r'null'
t_hex = r'((0[xX]([0-9a-fA-F][0-9a-fA-F_]*[0-9a-fA-F]|[0-9a-fA-F])\.([0-9a-fA-F][0-9a-fA-F_]*[0-9a-fA-F]|[0-9a-fA-F])|0[xX]([0-9a-fA-F][0-9a-fA-F_]*[0-9a-fA-F]|[0-9a-fA-F])\.|0[xX]\.([0-9a-fA-F][0-9a-fA-F_]*[0-9a-fA-F]|[0-9a-fA-F]))[pP][+-]([0-9][0-9_]*)[fFdD]?|0[xX]([0-9a-fA-F][0-9a-fA-F_]*[0-9a-fA-F]|[0-9a-fA-F])[lL]?)'
t_ch = r'\'([^\'\\\n\r]|(\\n|\\b|\\f|\\\"|\\\\|\\\'|\\r|\\t))\''
t_string = r'\"([^\"\\\n\r]*(\\n|\\t|\\b|\\f|\\r|\\\"|\\\\|\\\')*)*\"' 


identifier= r'[A-Za-z_][A-Za-z_0-9]*'
keydict={key:key for key in keywords+('false','true')}
# checking for reserved words
@lex.TOKEN(identifier)
def t_id(t):
	t.type=keydict.get(t.value,'id')
	return t

# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'

# Define a rule so we can track line numbers
def t_newline(t):
     r'\n+'
     t.lexer.lineno += len(t.value)

# Error handling rule
def t_error(t):
	t.type="ERROR"
	t.lexer.skip(1)
	print("ERROR : There has been a tokenisaion error somewhere in line number " + str(t.lexer.lineno) + ' for this token '+ str(t.value))
	# return t

lex.lex()  

file=open(sys.argv[1],'r')
test=file.read()
lex.input(test)
tok=lex.token()
while tok!=None:
 # print(tok)
 if tok.type == 'ERROR':
 	# print('Illegal token')
 	break
 	tok=lex.token()
 	continue
 if tok.type in ('lcomment','bcomment'):
 	tok =lex.token()
 	continue
 if tok.type in keywords:
 	category = 'Keyword'
 elif tok.type in separators:
 	category = 'Separator'
 elif tok.type in operators:
 	category = 'Operator'
 elif tok.type in liters:
 	category = 'Literal'
 else:
 	category = 'Identifier'
 if not tok: break
 print(str(tok.value) + '    '+ category)
 tok = lex.token()