#JAVA_PARSER_Complier_Project

import yacc
from lexer import lexer,tokens

################################################################################

def p_Identifier(p):
	'''Identifier : IDENTIFIER'''

def p_QualifiedIdentifier(p):
	'''QualifiedIdentifier : Identifier DOT QualifiedIdentifier 
						| Identifier'''

def p_QualifiedIdentifierList(p):
	'''QualifiedIdentifierList : QualifiedIdentifier COMMA QualifiedIdentifierList
							   | QualifiedIdentifier'''

################################################################################

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

################################################################################


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
def p_NonWildcardTypeArguments(p):
	'''NonWildcardTypeArguments : LESSTHAN TypeList GREATERTHAN '''

def p_TypeList(p):
	''' TypeList : ReferenceType COMMA TypeList 
				 | ReferenceType'''

def p_TypeArgumentsOrDiamond(p):
	'''TypeArgumentsOrDiamond : LESSTHAN GREATERTHAN
							  | TypeArguments '''

def p_NonWildcardTypeArgumentsOrDiamond(p):
	'''NonWildcardTypeArgumentsOrDiamond : LESSTHAN GREATERTHAN
							  | NonWildcardTypeArguments '''

def p_TypeParameters(p):
	''' TypeParameters : LESSTHAN TypeParametersList GREATERTHAN '''

def p_TypeParametersList(p):
	'''TypeParameters : TypeParameter COMMA TypeParameters
					  | TypeParameter'''


def p_TypeParameter(p):
	'''TypeParameter : Identifier EXTENDS Bound
					 | Identifier'''
def p_Bound(p):
	''' Bound : ReferenceType BOOLEANAND ReferenceType
			  | ReferenceType'''

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

def p_Expression(p):
	'''Expression : Expression1 AssignmentOperator Expression1
				  | Expression1'''
def p_AssignmentOperator(p):
	''' AssignmentOperator : EQUAL
						   | PLUSEQUALS
						   | MINUSEQUALS
						   | MULTIPLYEQUALS
						   | DIVIDEEQUALS
						   | ANDEQUALS
						   | OREQUALS
						   | XOREQUALS
						   | MODULOEQUALS
						   | LEFTSHIFTEQUALS
						   | RIGHTSHIFTEQUALS
						   | URIGHTSHIFTEQUALS'''

def p_Expression1(p):
	''' Expression1 : Expression2 Expression1Rest
					| Expression2'''
def p_Expression1Rest(p):
	'''Expression1Rest : QUESTIONMARK Expression COLON Expression1'''

def p_Expression2(p):
	'''Expression2 : Expression3 Expression2Rest
				   | Expression3'''

def p_Expression2Rest(p):
	''' Expression2Rest : infixoplist
						| INSTANCEOF Type'''
def p_infixoplist(p):
	''' infixoplist : InfixOp Expression3 infixoplist
					| empty'''

################################################################################

def p_InfixOp(p):
	''' InfixOp : OR
				| AND
				| BOOLEANOR
				| BOOLEANAND
				| BOOLEANXOR
				| EQUALS
				| NOTEQUALS
				| LESSTHAN
				| GREATERTHAN
				| LESSTHANEQUAL
				| GREATERTHANEQUAL
				| LEFTSHIFT
				| RIGHTSHIFT
				| URIGHTSHIFT
				| PLUS
				| MINUS
				| MULTIPLY
				| DIVIDE
				| MODULO'''

def p_Expression3(p):
	''' Expression3 : PrefixOp Expression3
					| LPAREN Expression RPAREN Expression3
					| LPAREN Type RPAREN Expression3
					| Primary selectorlist postfixoplist'''

def p_selectorlist(p):
	'''selectorlist : Selector selectorlist
					| empty'''

def p_postfixoplist(p):
	'''postfixoplist : PostfixOp postfixoplist
					 | empty'''

def p_PrefixOp(p):
	'''PrefixOp : PLUSPLUS
				| MINUSMINUS
				| BOOLEANNOT
				| TILDA 
				| PLUS
				| MINUS'''
def p_PostfixOp(p):
	'''PostfixOp : PLUSPLUS
				 | MINUSMINUS'''			

################################################################################

def p_Primary(p):
	'''Primary : Literal
			   | ParExpression
			   | this Arguments
			   | this
			   | SUPER SuperSuffix
			   | NEW Creator
			   | VOID DOT CLASS
			   | BasicType Brackets DOT CLASS
			   | NonWildcardTypeArguments ExplicitGenericInvocationSuffix
			   | NonWildcardTypeArguments this Arguments
			   | QualifiedIdentifier IdentifierSuffix
			   | QualifiedIdentifier'''

def p_Literal(p):
	'''Literal : IntegerLiteral
			   | FloatingPointLiteral
			   | CharacterLiteral
			   | StringLiteral
			   | BooleanLiteral
			   | NullLiteral'''

def p_ParExpression(p):
	'''ParExpression : LPAREN Expression RPAREN '''

def p_Arguments(p):
	''' Arguments : LPAREN RPAREN
				  | LPAREN expressionlist RPAREN '''

def p_expressionlist(p):
	''' expressionlist : Expression COMMA expressionlist
					   | Expression'''
def p_SuperSuffix(p):
	''' SuperSuffix : Arguments
					| DOT Identifier Arguments
					| DOT Identifier '''

def p_ExplicitGenericInvocationSuffix(p): 
	''' ExplicitGenericInvocationSuffix : SUPER SuperSuffix
										| Identifier Arguments '''

################################################################################

def p_Creator(p):
	'''Creator : NonWildcardTypeArguments CreatedName ClassCreatorRest
			   | CreatedName ClassCreatorRest
			   | CreatedName ArrayCreatorRest'''


def p_CreatedName(p):
	''' CreatedName : Identifier TypeArgumentsOrDiamond dotidtypelist
					| Identifier TypeArgumentsOrDiamond dotidlist
					| Identifier dotidtypelist
					| QualifiedIdentifier '''

def p_dotidlist(p):
	'''dotidlist : DOT Identifier dotidlist
				 | empty '''

def p_dotidtypelist(p):
	''' dotidtypelist : DOT Identifier TypeArgumentsOrDiamond dotidtypelist
					  | empty '''


def p_ClassCreatorRest(p);
	''' ClassCreatorRest : Arguments ClassBody
						 | Arguments'''

def p_ArrayCreatorRest(p):
	''' ArrayCreatorRest: LBRACKETS RBRACKETS Brackets ArrayInitializer  
						| LBRACKETS Expression RBRACKETS ExpList Brackets'''
def p_ExpList(p):
	''' ExpList : LBRACKETS Expression RBRACKETS ExpList
				| empty '''

def p_IdentifierSuffix(p):
	''' IdentifierSuffix : Arguments 
						 | LBRACKETS Expression RBRACKETS
						 | LBRACKETS Brackets DOT CLASS RBRACKETS
						 | DOT CLASS
						 | DOT THIS
						 | DOT ExplicitGenericInvocation
						 | DOT SUPER Arguments
						 | DOT NEW NonWildcardTypeArguments InnerCreator
						 | DOT NEW InnerCreator'''

def p_ExplicitGenericInvocation(p):
	' ExplicitGenericInvocation : NonWildcardTypeArguments ExplicitGenericInvocationSuffix'

def p_InnerCreator(p):
	'''InnerCreator :  Identifier NonWildcardTypeArgumentsOrDiamond ClassCreatorRest
					|  Identifier ClassCreatorRest'''

def p_Selector(p):
	''' Selector : DOT Identifier 
				 | DOT Identifier Arguments
				 | DOT ExplicitGenericInvocation
				 | DOT THIS
				 | DOT SUPER Arguments
				 | DOT NEW NonWildcardTypeArguments InnerCreator
			     | DOT NEW InnerCreator
			     | LBRACKETS Expression RBRACKETS''' 

################################################################################

def p_EnumBody(p):
	'''EnumBody : EnumBodyUnit EnumBody
				| empty'''  

def p_EnumBodyUnit(p):
	'''EnumBodyUnit : EnumConstants COMMA EnumBodyDeclarations
					| EnumConstants EnumBodyDeclarations
					| EnumConstants COMMA 
					| EnumConstants 
					| COMMA EnumBodyDeclarations
					| EnumBodyDeclarations
					| COMMA '''

def p_EnumConstants(p):
	'''EnumConstants : EnumConstant
					 | EnumConstants COMMA EnumConstant'''

def p_EnumConstant(p):
	'''EnumConstant : Annotations Identifier Arguments ClassBody
					| Annotations Identifier Arguments
					| Annotations Identifier ClassBody
					| Annotations Identifier
					| Identifier Arguments ClassBody
					| Identifier Arguments
					| Identifier ClassBody
					| Identifier'''

def p_EnumBodyDeclarations(p):
	''' EnumBodyDeclarations : SEMICOLON classbodylist'''

def p_classbodylist(p):
	'''classbodylist : ClassBodyDeclaration classbodylist
					 | empty'''

def p_AnnotationTypeBody(p):
	''' AnnotationTypeBody : LBRACES AnnotationTypeElementDeclarations RBRACES
						   | LBRACES RBRACES '''

def p_AnnotationTypeElementDeclarations(p):
	''' AnnotationTypeElementDeclarations : AnnotationTypeElementDeclaration
										  | AnnotationTypeElementDeclarations AnnotationTypeElementDeclaration'''
def p_AnnotationTypeElementDeclaration(p):
	''' AnnotationTypeElementDeclaration : ModifierList AnnotationTypeElementRest '''

def p_AnnotationTypeElementRest(p):
	''' AnnotationTypeElementRest : Type Identifier AnnotationMethodOrConstantRest SEMICOLON
								  | ClassDeclaration
								  | InterfaceDeclaration
								  | EnumDeclaration
								  | AnnotationTypeDeclaration'''

def p_AnnotationMethodOrConstantRest(p):
	''' AnnotationMethodOrConstantRest : AnnotationMethodRest
									   | ConstantDeclaratorsRest  '''


def p_AnnotationMethodRest(p):
	'''AnnotationMethodRest : LPAREN RPAREN [[]] DEFAULT ElementValue
							| LPAREN RPAREN [[]]'''


################################################################################

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
