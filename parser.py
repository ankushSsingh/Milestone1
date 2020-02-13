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
def p_CompilationUnit(p):
	TODO

def p_ImportDeclaration(p):
	TODO

def p_TypeDeclaration(p):
	'''TypeDeclaration : ClassOrInterfaceDeclaration
					   | SEMICOLON'''

def p_ClassOrInterfaceDeclaration(p):
	'''ClassOrInterfaceDeclaration : ModifierList ClassDeclaration
								   | ModifierList InterfaceDeclaration '''

def p_ModifierList(p):
	'''ModifierList : Modifier ModifierList
					| empty''' 


def p_ClassDeclaration(p):
	'''ClassDeclaration : NormalClassDeclaration
						| EnumDeclaration'''

def p_InterfaceDeclaration(p):
	''' InterfaceDeclaration: NormalInterfaceDeclaration
        					| AnnotationTypeDeclaration'''
def p_EnumDeclaration(p):
	''' EnumDeclaration : ENUM IDENTIFIER EnumBody 
						| ENUM IDENTIFIER implements TypeList EnumBody'''

def p_AnnotationTypeDeclaration(p):
	''' AnnotationTypeDeclaration: @ interface IDENTIFIER AnnotationTypeBody'''

def p_Type(p):
	''' Type : BasicType Brackets
			 | ReferenceType Brackets'''

def p_Brackets(p):
	''' Brackets : LBRACKETS RBRACKETS BRACKETS
				 | empty'''

def p_BasicType(p):
	''' BasicType : byte
				  | short
				  | char
				  | int
				  | long
				  | float
				  | double
				  | boolean '''

def p_TypeArguments(p):
	'''TypeArguments: LESSTHAN TypeArgumentList GREATERTHAN '''

def p_TypeArgumentList(p):
	'''TypeArgumentList : TypeArgument COMMA TypeArgumentList
					   | TypeArgument'''

def p_TypeArgument(p):
	'''TypeArgument : ReferenceType
					| QUESTIONMARK EXTENDS ReferenceType
					| QUESTIONMARK SUPER ReferenceType
					| QUESTIONMARK '''

def p_ReferenceType(p):
	TODO

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

################################################################################

#Default Error
def p_error(p):
    print("Input Error")
    return

#Empty_Production
def p_empty(p):
     'empty :'































# Build the parser
parser = yacc.yacc()
