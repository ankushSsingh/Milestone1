#JAVA_PARSER_Complier_Project

import yacc
from lexer import lexer,tokens


def p_Identifier(p):
	'''Identifier : IDENTIFIER'''

def p_QualifiedIdentifier(p):
	'''QualifiedIdentifier : Identifier DOT QualifiedIdentifier 
						| Identifier'''

def p_QualifiedIdentifierList(p):
	'''QualifiedIdentifierList : QualifiedIdentifier COMMA QualifiedIdentifierList
							   | QualifiedIdentifier'''
def p_TypeDeclaration(p):
	'''TypeDeclaration : ClassOrInterfaceDeclaration
					   | SEMICOLON'''
def p_ClassDeclaration(p):
	'''ClassDeclaration : NormalClassDeclaration
						| EnumDeclaration'''
def p_InterfaceDeclaration(p):
	''' InterfaceDeclaration: NormalInterfaceDeclaration
        			| AnnotationTypeDeclaration'''



#Default Error
def p_error(p):
    print("Input Error")
    return


# Build the parser
parser = yacc.yacc()
