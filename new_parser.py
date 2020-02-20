#JAVA_PARSER_Complier_Project

import yacc
from lexer import lexer,tokens

################################################################################

# TODO: Support for unicode needs to be added
def p_Identifier(p):
    '''Identifier : IDENTIFIER'''

# TODO: Support for unicode needs to be added
#  JavaLetterOrDigit:
#  any Unicode character that is a "Java letter-or-digit"
def p_JavaLetterOrDigit(p):
    '''JavaLetterOrDigit :  '''

def p_Literal(p):
    '''Literal : IntegerLiteral
               | FloatingPointLiteral
               | BooleanLiteral
               | CharacterLiteral
               | StringLiteral
               | NullLiteral'''

################################################################################

def p_Type(p):
    '''Type : PrimitiveType
            | ReferenceType'''


def p_PrimitiveType(p):
    '''PrimitiveType : MultAnnotation NumericType
                     | MultAnnotation boolean
                     | NumericType
                     | boolean'''

def p_NumericType(p):
    '''NumericType : | IntegralType
                     | FloatingPointType'''

def p_IntegralType(p):
    '''IntegralType : BYTE
                    | SHORT
                    | INT
                    | LONG
                    | CHAR'''

def p_FloatingPointType(p):
    '''FloatingPointType : FLOAT
                         | DOUBLE'''

def p_ReferenceType(p):
    '''ReferenceType : | ClassOrInterfaceType
                       | TypeVariable
                       | ArrayType'''


def p_ClassOrInterfaceType(p):
    '''ClassOrInterfaceType : ClassType
                            | InterfaceType'''


def p_ClassType(p):
    '''ClassType : ClassOrInterfaceType DOT MultAnnotation Identifier TypeArguments
                 | ClassOrInterfaceType DOT MultAnnotation Identifier
                 | ClassOrInterfaceType DOT Identifier TypeArguments
                 | ClassOrInterfaceType DOT Identifier
                 | MultAnnotation Identifier TypeArguments
                 | MultAnnotation Identifier
                 | Identifier TypeArguments
                 | Identifier
                 '''

def p_InterfaceType(p):
    '''InterfaceType : ClassType'''


def p_TypeVariable(p):
    '''TypeVariable : MultAnnotation Identifier
                    | Identifier'''

def p_ArrayType(p):
    '''ArrayType : PrimitiveType Dims
                 | ClassOrInterfaceType Dims
                 | TypeVariable Dims'''

def p_Dims(p):
    '''Dims : MultAnnotation LBRACKETS RBRACKETS Dims
            | MultAnnotation LBRACKETS RBRACKETS'''

def p_TypeParameter(p):
    '''TypeParameter : TypeParameterModifier Identifier TypeBound
                     | TypeParameterModifier Identifier
                     | Identifier TypeBound
                     | Identifier '''


def p_TypeParameterModifier(p):
    '''TypeParameterModifier : Annotation'''

def p_TypeBound(p):
    '''TypeBound : EXTENDS TypeVariable
                 | EXTENDS ClassOrInterfaceType AdditionalBound
                 | EXTENDS ClassOrInterfaceType '''

def p_AdditionalBound(p):
    '''AdditionalBound : BOOLEANAND InterfaceType'''

def p_TypeArguments(p):
    '''TypeArguments : LESSTHAN TypeArgumentList GREATERTHAN'''

def p_TypeArgumentList(p):
    '''TypeArgumentList : TypeArgument COMMA TypeArgumentList
                        | TypeArgument'''

def p_TypeArgument(p):
    '''TypeArgument : ReferenceType
                    | Wildcard'''

def p_Wildcard(p):
    '''Wildcard : MultAnnotation QUESTIONMARK WildcardBounds
                | MultAnnotation QUESTIONMARK
                | QUESTIONMARK WildcardBounds
                | QUESTIONMARK '''

def p_WildcardBounds(p):
    '''WildcardBounds : EXTENDS ReferenceType
                      | SUPER ReferenceType'''

################################################################################

def p_TypeName(p):
    '''TypeName : Identifier
                | PackageOrTypeName DOT Identifier'''

def p_PackageOrTypeName(p):
    '''PackageOrTypeName :  Identifier
                         | PackageOrTypeName DOT Identifier'''

def p_ExpressionName(p):
    '''ExpressionName : Identifier
                      | AmbiguousName DOT Identifier'''

def p_MethodName(p):
    '''MethodName :  Identifier'''

def p_PackageName(p):
    '''PackageName :  Identifier
                   | PackageName DOT Identifier '''

def p_AmbiguousName(p):
    '''AmbiguousName : Identifier
                     | AmbiguousName DOT Identifier '''

################################################################################

def p_CompilationUnit(p):
    '''CompilationUnit : PackageDeclaration MultImportDeclaration TypeDeclaration
                       | PackageDeclaration MultImportDeclaration
                       | PackageDeclaration TypeDeclaration
                       | MultImportDeclaration TypeDeclaration
                       | TypeDeclaration
                       | PackageDeclaration
                       | MultImportDeclaration
                       | empty
                       '''

def p_MultImportDeclaration(p):
    '''MultImportDeclaration : ImportDeclaration MultImportDeclaration
                             | ImportDeclaration'''

def p_MultTypeDeclaration(p):
    '''MultTypeDeclaration : TypeDeclaration MultTypeDeclaration
                           | TypeDeclaration'''

def p_PackageDeclaration(p):
    '''PackageDeclaration : MultPackageModifier PACKAGE DotSeparatedIdentifiers SEMICOLON
                          | PACKAGE DotSeparatedIdentifiers SEMICOLON'''

def p_MultPackageModifier(p):
    '''MultPackageModifier : PackageModifier MultPackageModifier
                           | PackageModifier'''

def p_PackageModifier(p):
    '''PackageModifier : Annotation'''


def p_ImportDeclaration(p):
    '''ImportDeclaration : SingleTypeImportDeclaration
                         | TypeImportOnDemandDeclaration
                         | SingleStaticImportDeclaration
                         | StaticImportOnDemandDeclaration '''

def p_SingleTypeImportDeclaration(p):
    '''SingleTypeImportDeclaration : IMPORT TypeName SEMICOLON '''


def p_TypeImportOnDemandDeclaration(p):
    '''TypeImportOnDemandDeclaration : IMPORT PackageOrTypeName DOT MULTIPLY t_SEMICOLON'''

def p_SingleStaticImportDeclaration(p):
    '''SingleStaticImportDeclaration : IMPORT STATIC TypeName DOT Identifier SEMICOLON '''

def p_StaticImportOnDemandDeclaration(p):
    '''StaticImportOnDemandDeclaration :  IMPORT STATIC TypeName DOT MULTIPLY SEMICOLON'''

def p_TypeDeclaration(p):
    '''TypeDeclaration : ClassDeclaration
                       | InterfaceDeclaration
                       | SEMICOLON'''

################################################################################

def p_ClassDeclaration(p):
    '''ClassDeclaration : NormalClassDeclaration
                        | EnumDeclaration'''

def p_NormalClassDeclaration(p):
    '''NormalClassDeclaration : MultClassModifier CLASS Identifier TypeParameters Superclass Superinterfaces ClassBody
                              | MultClassModifier CLASS Identifier TypeParameters Superclass ClassBody
                              | MultClassModifier CLASS Identifier TypeParameters Superinterfaces ClassBody
                              | MultClassModifier CLASS Identifier Superclass Superinterfaces ClassBody
                              | MultClassModifier CLASS Identifier TypeParameters ClassBody
                              | MultClassModifier CLASS Identifier Superclass ClassBody
                              | MultClassModifier CLASS Identifier Superinterfaces ClassBody
                              | MultClassModifier CLASS Identifier ClassBody
                              | CLASS Identifier TypeParameters Superclass Superinterfaces ClassBody
                              | CLASS Identifier TypeParameters Superclass ClassBody
                              | CLASS Identifier TypeParameters Superinterfaces ClassBody
                              | CLASS Identifier Superclass Superinterfaces ClassBody
                              | CLASS Identifier TypeParameters ClassBody
                              | CLASS Identifier Superclass ClassBody
                              | CLASS Identifier Superinterfaces ClassBody
                              | CLASS Identifier ClassBody
                              '''

def p_MultClassModifier(p):
    '''MultClassModifier : ClassModifier MultClassModifier
                         | ClassModifier'''

def p_ClassModifier(p):
    '''ClassModifier : ClassModifier1
                     | ABSTRACT
                     | STATIC
                     | FINAL
                     | STRICTFP'''

def p_ClassModifier1(p):
    '''ClassModifier1 : Annotation
                      | PUBLIC
                      | PROTECTED
                      | PRIVATE'''

def p_TypeParameters(p):
    '''TypeParameters : LESSTHAN TypeParameterList GREATERTHAN'''


def p_TypeParameterList(p):
    '''TypeParameterList : TypeParameter COMMA TypeParameterList
                         | TypeParameter'''

def p_Superclass(p):
    '''Superclass : EXTENDS ClassType'''


def p_Superinterfaces(p):
    '''Superinterfaces : IMPLEMENTS InterfaceTypeList '''


def p_InterfaceTypeList(p):
    '''InterfaceTypeList : InterfaceType {, InterfaceType}'''

def p_InterfaceTypeList(p):
    '''InterfaceTypeList : TypeParameter COMMA InterfaceTypeList
                         | TypeParameter'''

def p_ClassBody(p):
    '''ClassBody : LBRACES MultClassBodyDeclaration RBRACES
                 | LBRACES RBRACES'''

def p_MultClassBodyDeclaration(p):
    '''MultClassBodyDeclaration : ClassBodyDeclaration MultClassBodyDeclaration
                                | ClassBodyDeclaration'''

def p_ClassBodyDeclaration(p):
    '''ClassBodyDeclaration :  ClassMemberDeclaration
                            | InstanceInitializer
                            | StaticInitializer
                            | ConstructorDeclaration'''

def p_ClassMemberDeclaration(p):
    '''ClassMemberDeclaration : FieldDeclaration
                              | MethodDeclaration
                              | ClassDeclaration
                              | InterfaceDeclaration
                              | SEMICOLON '''

def p_FieldDeclaration(p):
    '''FieldDeclaration : MultFieldModifier UnannType VariableDeclaratorList SEMICOLON
                        | UnannType VariableDeclaratorList SEMICOLON'''

def p_MultFieldModifier(p):
    '''MultFieldModifier : FieldModifier MultFieldModifier
                         | FieldModifier'''

def p_FieldModifier(p):
    '''FieldModifier : FieldModifier1
                     | STATIC
                     | FINAL
                     | TRANSIENT
                     | VOLATILE'''

def p_FieldModifier1(p):
    '''FieldModifier1 : Annotation PUBLIC PROTECTED PRIVATE'''

def p_VariableDeclaratorList(p):
    '''VariableDeclaratorList : VariableDeclarator COMMA VariableDeclaratorList
                              | VariableDeclarator '''

def p_VariableDeclarator(p):
    '''VariableDeclarator : VariableDeclaratorId EQUAL VariableInitializer
                          | VariableDeclaratorId'''

def p_VariableDeclaratorId(p):
    '''VariableDeclaratorId : Identifier Dims
                            | Identifier'''

def p_VariableInitializer(p):
    '''VariableInitializer : Expression
                           | ArrayInitializer '''


def p_UnannType(p):
    '''UnannType : UnannPrimitiveType
                 | UnannReferenceType '''


def p_UnannPrimitiveType(p):
    '''UnannPrimitiveType : NumericType
                          | boolean'''


def p_UnannReferenceType(p):
    '''UnannReferenceType : UnannClassOrInterfaceType
                          | UnannTypeVariable
                          | UnannArrayType '''

def p_UnannClassOrInterfaceType(p):
    '''UnannClassOrInterfaceType : UnannClassType
                                 | UnannInterfaceType '''


def p_UnannClassType(p):
    '''UnannClassType : Identifier TypeArguments
                      | Identifier
                      | UnannClassOrInterfaceType DOT MultAnnotation Identifier TypeArguments
                      | UnannClassOrInterfaceType DOT MultAnnotation Identifier
                      | UnannClassOrInterfaceType DOT Identifier TypeArguments
                      | UnannClassOrInterfaceType DOT Identifier
                      '''
def p_UnannInterfaceType(p):
    '''UnannInterfaceType :  UnannClassType'''

def p_UnannTypeVariable(p):
    '''UnannTypeVariable : Identifier '''


def p_UnannArrayType(p):
    '''UnannArrayType : UnannPrimitiveType Dims
                      | UnannClassOrInterfaceType Dims
                      | UnannTypeVariable Dims '''



def p_MethodDeclaration(p):
    '''MethodDeclaration : MultMethodModifier MethodHeader MethodBody
                         | MethodHeader MethodBody '''

def p_MultMethodModifier(p):
    '''MultMethodModifier : MethodModifier MultMethodModifier
                      | MethodModifier'''


def p_MethodModifier(p):
    '''MethodModifier : MethodModifier1
                      | ABSTRACT
                      | STATIC
                      | FINAL
                      | SYNCHRONIZED
                      | NATIVE
                      | STRICTFP '''

def p_MethodModifier1(p):
    '''MethodModifier1 : Annotation
                       | PUBLIC
                       | PROTECTED
                       | PRIVATE'''
def p_MethodHeader(p):
    '''MethodHeader : Result MethodDeclarator Throws
                    | Result MethodDeclarator
                    | TypeParameters MultAnnotation Result MethodDeclarator Throws
                    | TypeParameters MultAnnotation Result MethodDeclarator
                    | TypeParameters Result MethodDeclarator Throws
                    | TypeParameters Result MethodDeclarator
                    '''

def p_Result(p):
    '''Result : UnannType
              | VOID'''

def p_MethodDeclarator(p):
    '''MethodDeclarator : Identifier LPAREN FormalParameterList RPAREN Dims
                        | Identifier LPAREN FormalParameterList RPAREN
                        | Identifier LPAREN RPAREN Dims
                        | Identifier LPAREN RPAREN'''

def p_FormalParameterList(p):
    '''FormalParameterList : ReceiverParameter
                           | FormalParameters COMMA LastFormalParameter
                           | LastFormalParameter '''

def p_FormalParameters(p):
    '''FormalParameters : FormalParameter COMMA FormalParameters
                        | FormalParameter
                        | ReceiverParameter COMMA FormalParameters
                        | FormalParameter'''

def p_FormalParameter(p):
    '''FormalParameter : {VariableModifier} UnannType VariableDeclaratorId '''


def p_VariableModifier(p):
    '''VariableModifier :  '''
(one of)
Annotation final

LastFormalParameter:
{VariableModifier} UnannType {MultAnnotation} ... VariableDeclaratorId
FormalParameter

ReceiverParameter:
{MultAnnotation} UnannType [Identifier .] this

Throws:
throws ExceptionTypeList

ExceptionTypeList:
ExceptionType {, ExceptionType}

ExceptionType:
ClassType
TypeVariable

MethodBody:
Block
;

InstanceInitializer:
Block

StaticInitializer:
static Block

ConstructorDeclaration:
{ConstructorModifier} ConstructorDeclarator [Throws] ConstructorBody

ConstructorModifier:
(one of)
Annotation public protected private

ConstructorDeclarator:
[TypeParameters] SimpleTypeName ( [FormalParameterList] )

SimpleTypeName:
Identifier

ConstructorBody:
{ [ExplicitConstructorInvocation] [BlockStatements] }

ExplicitConstructorInvocation:
[TypeArguments] this ( [ArgumentList] ) ;
[TypeArguments] super ( [ArgumentList] ) ;
ExpressionName . [TypeArguments] super ( [ArgumentList] ) ;
Primary . [TypeArguments] super ( [ArgumentList] ) ;

EnumDeclaration:
{ClassModifier} enum Identifier [Superinterfaces] EnumBody

EnumBody:
{ [EnumConstantList] [,] [EnumBodyDeclarations] }

EnumConstantList:
EnumConstant {, EnumConstant}

EnumConstant:
{EnumConstantModifier} Identifier [( [ArgumentList] )] [ClassBody]

EnumConstantModifier:
Annotation

EnumBodyDeclarations:
; {ClassBodyDeclaration}

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

def p_MultAnnotation(p):
    '''MultAnnotation : Annotation MultAnnotation
                      | Annotation'''

def p_DotSeparatedIdentifiers(p):
    '''DotSeparatedIdentifiers : Identifier DOT DotSeparatedIdentifiers
                               | Identifier'''


# Build the parser
parser = yacc.yacc()

