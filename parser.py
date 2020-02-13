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


################################################################################

def p_Modifier(p):
    '''Modifier : Annotation
            |    PUBLIC
            |    PROTECTED
            |    PRIVATE
            |    STATIC 
            |    ABSTRACT
            |    FINAL
            |    NATIVE
            |    SYNCHRONIZED
            |    TRANSIENT
            |    VOLATILE
            |    STRICTFP '''

def p_Annotations(p):
    '''Annotations : Annotation Annotations
                   | Annotation'''

# TODO: Annotation:
#  @ QualifiedIdentifier [ ( [AnnotationElement] ) ]
def p_Annotation(p):
    '''Annotation : AT QualifiedIdentifier AnnotationElement
                  | AT QualifiedIdentifier '''

def p_AnnotationElement(p):
    '''AnnotationElement :  ElementValuePairs
                         | ElementValue '''

def p_ElementValuePairs(p):
    '''ElementValuePairs : ElementValuePair COMMA ElementValuePairs
                         | ElementValuePair '''

def p_ElementValuePair(p):
    '''ElementValuePair : IDENTIFIER EQUAL ElementValue '''

def p_ElementValue(p):
    '''ElementValue :  Annotation
                    | Expression1
                    | ElementValueArrayInitializer'''

# TODO:
def p_ElementValueArrayInitializer(p):
    '''ElementValueArrayInitializer :  '''


def p_ElementValues(p):
    '''ElementValues : ElementValue ElementValues
                     | ElementValue'''

################################################################################

# TODO: 
def p_ClassBody(p):
    '''ClassBody : LBRACES ClassBodyDeclaration RBRACES '''

def p_ClassBodyDeclaration(p):
    '''ClassBodyDeclaration : SEMICOLON
                            | Modifiers MemberDecl
                            | STATIC Block
                            | Block'''
def p_Modifiers(p):
    '''Modifiers : Modifier Modifiers
                 | empty '''

def p_MemberDecl(p):
    '''MemberDecl : MethodOrFieldDecl
                  |    VOID IDENTIFIER VoidMethodDeclaratorRest
                  |    IDENTIFIER ConstructorDeclaratorRest
                  |    GenericMethodOrConstructorDecl
                  |    ClassDeclaration
                  |    InterfaceDeclaration '''

def p_MethodOrFieldDecl(p):
    '''MethodOrFieldDecl : Type IDENTIFIER MethodOrFieldRest'''

def p_MethodOrFieldRest(p):
    '''MethodOrFieldRest : FieldDeclaratorsRest SEMICOLON
                         | MethodDeclaratorRest'''

def p_FieldDeclaratorsRest(p):
    '''FieldDeclaratorsRest : VariableDeclaratorRest COMMA VariableDeclarator
                            | VariableDeclaratorRest '''

# TODO:
def p_MethodDeclaratorRest(p):
    '''MethodDeclaratorRest :  FormalParameters '''

FormalParameters {[]} [throws QualifiedIdentifierList] (Block | ;)
#Default Error
def p_error(p):
    print("Input Error")
    return































# Build the parser
parser = yacc.yacc()
