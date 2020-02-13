#JAVA_PARSER_Complier_Project

import yacc
from lexer import lexer,tokens


def p_Identifier(p):
	'''Identifier : IDENTIFIER'''

def p_QualifiedIdentifier(p):
	'QualifiedIdentifier : Identifier { . Identifier } '

#Default Error
def p_error(p):
    print("Input Error")
    return


# Build the parser
parser = yacc.yacc()
