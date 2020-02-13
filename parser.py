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
    '''ElementValuePair : Identifier EQUAL ElementValue '''

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
                            | ModifierList MemberDecl
                            | STATIC Block
                            | Block'''
def p_MemberDecl(p):
    '''MemberDecl : MethodOrFieldDecl
                  |    VOID Identifier VoidMethodDeclaratorRest
                  |    Identifier ConstructorDeclaratorRest
                  |    GenericMethodOrConstructorDecl
                  |    ClassDeclaration
                  |    InterfaceDeclaration '''

def p_MethodOrFieldDecl(p):
    '''MethodOrFieldDecl : Type Identifier MethodOrFieldRest'''

def p_MethodOrFieldRest(p):
    '''MethodOrFieldRest : FieldDeclaratorsRest SEMICOLON
                         | MethodDeclaratorRest'''

def p_FieldDeclaratorsRest(p):
    '''FieldDeclaratorsRest : VariableDeclaratorRest COMMA VariableDeclarator
                            | VariableDeclaratorRest '''

# TODO:
def p_MethodDeclaratorRest(p):
    '''MethodDeclaratorRest : FormalParameters Brackets THROWS QualifiedIdentifierList Block
                            | FormalParameters Brackets THROWS QualifiedIdentifierList SEMICOLON
                            | FormalParameters Brackets Block
                            | FormalParameters Brackets SEMICOLON'''

def p_VoidMethodDeclaratorRest(p):
    '''VoidMethodDeclaratorRest : FormalParameters THROWS QualifiedIdentifierList Block
                                | FormalParameters THROWS QualifiedIdentifierList SEMICOLON
                                | FormalParameters Block
                                | FormalParameters SEMICOLON'''

def p_ConstructorDeclaratorRest(p):
    '''ConstructorDeclaratorRest : FormalParameters THROWS QualifiedIdentifierList Block
                                 | FormalParameters Block'''

def p_GenericMethodOrConstructorDecl(p):
    '''GenericMethodOrConstructorDecl : TypeParameters GenericMethodOrConstructorRest'''

def p_GenericMethodOrConstructorRest(p):
    '''GenericMethodOrConstructorRest : Type Identifier MethodDeclaratorRest
                                      | VOID Identifier MethodDeclaratorRest
                                      | Identifier ConstructorDeclaratorRest'''

################################################################################

# TODO:
def p_InterfaceBody(p):
    '''InterfaceBody : LBRACES InterfaceBodyDeclaration RBRACES '''


def p_InterfaceBodyDeclaration(p):
    '''InterfaceBodyDeclaration : SEMICOLON 
                                | ModifierList InterfaceMemberDecl'''

def p_InterfaceMemberDecl(p):
    '''InterfaceMemberDecl : InterfaceMethodOrFieldDecl
                           | VOID Identifier VoidInterfaceMethodDeclaratorRest
                           | InterfaceGenericMethodDecl
                           | ClassDeclaration
                           | InterfaceDeclaration'''

def p_InterfaceMethodOrFieldDecl(p):
    '''InterfaceMethodOrFieldDecl :  Type Identifier InterfaceMethodOrFieldRest'''

def p_InterfaceMethodOrFieldRest(p):
    '''InterfaceMethodOrFieldRest : ConstantDeclaratorsRest SEMICOLON
                                  | InterfaceMethodDeclaratorRest '''

def p_ConstantDeclaratorsRest(p):
    '''ConstantDeclaratorsRest : ConstantDeclaratorRest COMMA ConstantDeclarator
                               | ConstantDeclaratorRest'''

def p_ConstantDeclaratorRest(p):
    '''ConstantDeclaratorRest : Brackets EQUAL VariableInitializer'''

def p_ConstantDeclarator(p):
    '''ConstantDeclarator :  '''
    Identifier ConstantDeclaratorRest

def p_InterfaceMethodDeclaratorRest(p):
    '''InterfaceMethodDeclaratorRest : FormalParameters Brackets THROWS QualifiedIdentifierList SEMICOLON
                                     | FormalParameters Brackets THROWS QualifiedIdentifierList SEMICOLON'''

def p_VoidInterfaceMethodDeclaratorRest(p):
    '''VoidInterfaceMethodDeclaratorRest :  FormalParameters THROWS QualifiedIdentifierList SEMICOLON
                                         | FormalParameters SEMICOLON'''

def p_InterfaceGenericMethodDecl(p):
    '''InterfaceGenericMethodDecl :  TypeParameters Type Identifier InterfaceMethodDeclaratorRest
                                  | TypeParameters VOID Identifier InterfaceMethodDeclaratorRest'''


def p_ConstantDeclaratorRest(p):
    '''ConstantDeclaratorRest : Brackets EQUAL VariableInitializer '''

def p_ConstantDeclarator(p):
    '''ConstantDeclarator :  Identifier ConstantDeclaratorRest'''

def p_InterfaceMethodDeclaratorRest(p):
    '''InterfaceMethodDeclaratorRest :  FormalParameters Brackets THROWS QualifiedIdentifierList SEMICOLON
    | FormalParameters Brackets SEMICOLON'''

def p_VoidInterfaceMethodDeclaratorRest(p):
    '''VoidInterfaceMethodDeclaratorRest :  FormalParameters THROWS QualifiedIdentifierList SEMICOLON
                                         | FormalParameters SEMICOLON'''

def p_InterfaceGenericMethodDecl(p):
    '''InterfaceGenericMethodDecl : TypeParameters Type Identifier InterfaceMethodDeclaratorRest
                                  | TypeParameters VOID Identifier InterfaceMethodDeclaratorRest'''

################################################################################

#TODO:
def p_FormalParameters(p):
    '''FormalParameters :  '''
    #  ( [FormalParameterDecls] )

def p_FormalParameterDecls(p):
    '''FormalParameterDecls : VariableModifier Type FormalParameterDeclsRest
                            | Type FormalParameterDeclsRest'''

def p_VariableModifier(p):
    '''VariableModifier : FINAL
                        | Annotation '''

def p_FormalParameterDeclsRest(p):
    '''FormalParameterDeclsRest : VariableDeclaratorId COMMA FormalParameterDecls
                                | ELLIPSIS VariableDeclaratorId
                                | VariableDeclaratorId'''

def p_VariableDeclaratorId(p):
    '''VariableDeclaratorId : Identifier Brackets'''

def p_VariableDeclarators(p):
    '''VariableDeclarators : VariableDeclarator COMMA VariableDeclarators
                           | VariableDeclarator'''
def p_VariableDeclarator(p):
    '''VariableDeclarator :  Identifier VariableDeclaratorRest'''

# TODO: 
def p_VariableDeclaratorRest(p):
    '''VariableDeclaratorRest : Brackets EQUAL VariableInitializer
                              | Brackets'''

def p_VariableInitializer(p):
    '''VariableInitializer : ArrayInitializer
                           | Expression'''

#TODO:
def p_ArrayInitializer(p):
    '''ArrayInitializer : LBRACES VariableInitializer MultipleVariableInitializer COMMA RBRACES
                        | LBRACES VariableInitializer MultipleVariableInitializer RBRACES
                        | LBRACES VariableInitializer COMMA RBRACES
                        | LBRACES VariableInitializer RBRACES
                        | LBRACES RBRACES'''

def p_MultipleVariableInitializer(p):
    '''MultipleVariableInitializer : MultipleVariableInitializer COMMA VariableInitializer
                                   | COMMA VariableInitializer'''

################################################################################

#TODO:
def p_Block(p):
    '''Block : LBRACES BlockStatements RBRACES'''

def p_BlockStatements(p):
    '''BlockStatements : LBRACES BlockStatement RBRACES'''

def p_BlockStatement(p):
    '''BlockStatement : LocalVariableDeclarationStatement
                      | ClassOrInterfaceDeclaration
                      | Identifier COLON Statement
                      | Statement '''

# TODO:
def p_LocalVariableDeclarationStatement(p):
    '''LocalVariableDeclarationStatement :  { VariableModifier }  Type VariableDeclarators ;'''

# TODO:
def p_Statement(p):
    '''Statement : Block
                        |    SEMICOLON
                        |    Identifier COLON Statement
                        |    StatementExpression SEMICOLON
                        |    IF ParExpression Statement ELSE Statement
                        |    IF ParExpression Statement
                        |    ASSERT Expression [COLON Expression] SEMICOLON
                        |    ASSERT Expression SEMICOLON
                        |    SWITCH ParExpression LBRACES SwitchBlockStatementGroups RBRACES
                        |    WHILE ParExpression Statement
                        |    DO Statement WHILE ParExpression SEMICOLON
                        |    FOR LPAREN ForControl RPAREN SEMICOLON Statement
                        |    break Identifier SEMICOLON
                        |    break SEMICOLON
                        |    CONTINUE Identifier SEMICOLON
                        |    CONTINUE SEMICOLON
                        |    RETURN Expression SEMICOLON
                        |    RETURN SEMICOLON
                        |    throw Expression SEMICOLON
                        |    SYNCHRONIZED ParExpression Block
                        |    TRY Block Catches Finally
                        |    TRY Block Catches
                        |    TRY Block Finally
                        |    TRY ResourceSpecification Block Catches Finally
                        |    TRY ResourceSpecification Block Catches
                        |    TRY ResourceSpecification Block Finally
                        |    TRY ResourceSpecification Block
        '''

def p_StatementExpression(p):
    '''StatementExpression :  Expression'''

################################################################################ 

def p_Catches(p):
    '''Catches : CatchClause LBRACES CatchClause RBRACES'''

def p_CatchClause(p):
    '''CatchClause : CATCH LPAREN VariableModifier CatchType Identifier RPAREN Block
                   | CATCH LPAREN CatchType Identifier RPAREN Block '''

def p_CatchType(p):
    '''CatchType :  MultipleQualifiedIdentifier { BOOLEANOR QualifiedIdentifier }'''

def p_MultipleQualifiedIdentifier(p):
    '''MultipleQualifiedIdentifier : QualifiedIdentifier BOOLEANOR MultipleQualifiedIdentifier
                                   | QualifiedIdentifier'''

def p_Finally(p):
    '''Finally :  FINALLY Block'''

def p_ResourceSpecification(p):
    '''ResourceSpecification : LPAREN Resources COMMA RPAREN
                             | LPAREN Resources RPAREN '''

def p_Resources(p):
    '''Resources : Resource SEMICOLON Resources
                 | Resource'''

def p_Resource(p):
    '''Resource : VariableModifier ReferenceType VariableDeclaratorId EQUAL Expression
                | ReferenceType VariableDeclaratorId EQUAL Expression'''

################################################################################ 

# TODO
def p_SwitchBlockStatementGroups(p):
    '''SwitchBlockStatementGroups : LBRACES SwitchBlockStatementGroup RBRACES'''

# TODO
def p_SwitchBlockStatementGroup(p):
    '''SwitchBlockStatementGroup : SwitchLabels BlockStatements'''

# TODO
def p_SwitchLabels(p):
    '''SwitchLabels :  SwitchLabel LBRACES SwitchLabel RBRACES'''

def p_SwitchLabel(p):
    '''SwitchLabel : CASE Expression COLON
                   | CASE EnumConstantName COLON
                   | DEFAULT COLON'''

def p_EnumConstantName(p):
    '''EnumConstantName : Identifier'''

def p_ForControl(p):
    '''ForControl : ForVarControl
                  | ForInit SEMICOLON Expression SEMICOLON ForUpdate
                  | ForInit SEMICOLON SEMICOLON ForUpdate
                  | ForInit SEMICOLON Expression SEMICOLON
                  | ForInit SEMICOLON SEMICOLON '''
# TODO:
def p_ForVarControl(p):
    '''ForVarControl : VariableModifier Type VariableDeclaratorId ForVarControlRest'''

# TODO:
#  ForVarControlRest:
    #  ForVariableDeclaratorsRest ; [Expression] ; [ForUpdate]
    #  : Expression

#  ForVariableDeclaratorsRest:
    #  [= VariableInitializer] { , VariableDeclarator }

#  ForInit: 
#  ForUpdate:
    #  StatementExpression { , StatementExpression }    

#Default Error
def p_error(p):
    print("Input Error")
    return

#Empty_Production
def p_empty(p):
     'empty :'

def p_Brackets(p):
	''' Brackets : LBRACKETS RBRACKETS BRACKETS
				 | empty'''

def p_ModifierList(p):
	'''ModifierList : Modifier ModifierList
					| empty''' 






























# Build the parser
parser = yacc.yacc()
