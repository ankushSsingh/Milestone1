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
                     | MultAnnotation BOOLEAN
                     | NumericType
                     | BOOLEAN'''

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

def p_MultAnnotation(p):
    '''MultAnnotation : Annotation MultAnnotation
                      | Annotation'''

def p_ArrayType(p):
    '''ArrayType : PrimitiveType Dims
                 | ClassOrInterfaceType Dims
                 | TypeVariable Dims'''

def p_Dims(p):
    '''Dims : MultAnnotation LBRACKETS RBRACKETS Dims
            | MultAnnotation LBRACKETS RBRACKETS'''

def p_TypeParameter(p):
    '''TypeParameter : MultTypeParameterModifier Identifier TypeBound
                     | MultTypeParameterModifier Identifier
                     | Identifier TypeBound
                     | Identifier '''

def p_MultTypeParameterModifier(p):
    '''MultTypeParameterModifier : MultTypeParameterModifier TypeParameterModifier
                                 | TypeParameterModifier'''

def p_TypeParameterModifier(p):
    '''TypeParameterModifier : Annotation'''

def p_TypeBound(p):
    '''TypeBound : EXTENDS TypeVariable
                 | EXTENDS ClassOrInterfaceType MultAdditionalBound
                 | EXTENDS ClassOrInterfaceType '''

def p_MultAdditionalBound(p):
  ''' MultAdditionalBound : MultAdditionalBound AdditionalBound
                          | AdditionalBound '''

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
    '''CompilationUnit : PackageDeclaration MultImportDeclaration MultTypeDeclaration
                       | PackageDeclaration MultImportDeclaration
                       | PackageDeclaration MultTypeDeclaration
                       | MultImportDeclaration MultTypeDeclaration
                       | MultTypeDeclaration
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

def p_DotSeparatedIdentifiers(p):
    '''DotSeparatedIdentifiers : Identifier DOT DotSeparatedIdentifiers
                               | Identifier'''

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
    '''TypeImportOnDemandDeclaration : IMPORT PackageOrTypeName DOT MULTIPLY SEMICOLON'''

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

# TODO: Ask about one of
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
    '''FormalParameter : MultVariableModifier UnannType VariableDeclaratorId
                       | UnannType VariableDeclaratorId  '''


def p_VariableModifier(p):
    '''VariableModifier : Annotation
                        | FINAL'''

def p_MultVariableModifier(p):
    '''MultVariableModifier : VariableModifier MultVariableModifier
                            | VariableModifier'''

def p_LastFormalParameter(p):
    '''LastFormalParameter : MultVariableModifier UnannType MultAnnotation ELLIPSIS VariableDeclaratorId
                           | MultVariableModifier UnannType ELLIPSIS VariableDeclaratorId
                           | UnannType MultAnnotation ELLIPSIS VariableDeclaratorId
                           | UnannType ELLIPSIS VariableDeclaratorId
                           | FormalParameter '''
def p_ReceiverParameter(p):
    '''ReceiverParameter : MultAnnotation UnannType Identifier DOT THIS
                         | MultAnnotation UnannType THIS
                         | UnannType Identifier DOT THIS
                         | UnannType THIS '''


def p_Throws(p):
    '''Throws : THROWS ExceptionTypeList'''

def p_ExceptionTypeList(p):
    '''ExceptionTypeList : ExceptionType COMMA ExceptionTypeList
                         | ExceptionType '''

def p_ExceptionType(p):
    '''ExceptionType :  ClassType
                     | TypeVariable'''


def p_MethodBody(p):
    '''MethodBody : Block
                  | SEMICOLON'''

def p_InstanceInitializer(p):
    '''InstanceInitializer :  Block'''


def p_StaticInitializer(p):
    '''StaticInitializer : STATIC Block'''


def p_ConstructorDeclaration(p):
    '''ConstructorDeclaration : ConstructorModifier ConstructorDeclarator Throws ConstructorBody '''

def p_ConstructorModifier(p):
    '''ConstructorModifier : Annotation
                           | PUBLIC
                           | PROTECTED
                           | PRIVATE '''
def p_MultConstructorModifier(p):
    '''MultConstructorModifier : ConstructorModifier MultConstructorModifier
                               | ConstructorModifier'''

def p_ConstructorDeclarator(p):
    '''ConstructorDeclarator : TypeParameters SimpleTypeName LPAREN FormalParameterList RPAREN
                             | TypeParameters SimpleTypeName LPAREN RPAREN
                             | SimpleTypeName LPAREN FormalParameterList RPAREN
                             | SimpleTypeName LPAREN RPAREN '''
def p_SimpleTypeName(p):
    '''SimpleTypeName : Identifier '''


def p_ConstructorBody(p):
    '''ConstructorBody : LBRACES ExplicitConstructorInvocation BlockStatements RBRACES
                       | LBRACES ExplicitConstructorInvocation RBRACES
                       | LBRACES BlockStatements RBRACES
                       | LBRACES RBRACES '''

def p_ExplicitConstructorInvocation(p):
    '''ExplicitConstructorInvocation : TypeArguments THIS LPAREN ArgumentList RPAREN SEMICOLON
                                     | TypeArguments THIS LPAREN RPAREN SEMICOLON
                                     | THIS LPAREN ArgumentList RPAREN SEMICOLON
                                     | THIS LPAREN RPAREN SEMICOLON
                                     | TypeArguments SUPER LPAREN ArgumentList RPAREN SEMICOLON
                                     | TypeArguments SUPER LPAREN RPAREN SEMICOLON
                                     | SUPER LPAREN ArgumentList RPAREN SEMICOLON
                                     | SUPER LPAREN RPAREN SEMICOLON
                                     | ExpressionName DOT TypeArguments SUPER LPAREN ArgumentList RPAREN SEMICOLON
                                     | ExpressionName DOT TypeArguments SUPER LPAREN RPAREN SEMICOLON
                                     | ExpressionName DOT SUPER LPAREN ArgumentList RPAREN SEMICOLON
                                     | ExpressionName DOT SUPER LPAREN RPAREN SEMICOLON
                                     | Primary DOT TypeArguments SUPER LPAREN ArgumentList RPAREN SEMICOLON
                                     | Primary DOT TypeArguments SUPER LPAREN RPAREN SEMICOLON
                                     | Primary DOT SUPER LPAREN ArgumentList RPAREN SEMICOLON
                                     | Primary DOT SUPER LPAREN RPAREN SEMICOLON '''

def p_EnumDeclaration(p):
    '''EnumDeclaration : ClassModifier ENUM Identifier Superinterfaces EnumBody
                       | ClassModifier ENUM Identifier EnumBody
                       | ENUM Identifier Superinterfaces EnumBody
                       | ENUM Identifier EnumBody '''

#TODO:
def p_EnumBody(p):
    '''EnumBody : LBRACES [EnumConstantList] [,] [EnumBodyDeclarations] RBRACES
                | LBRACES [EnumConstantList] [,] [EnumBodyDeclarations] RBRACES
                | LBRACES [EnumConstantList] [,] [EnumBodyDeclarations] RBRACES
                | LBRACES [EnumConstantList] [,] [EnumBodyDeclarations] RBRACES
                | LBRACES [EnumConstantList] [,] [EnumBodyDeclarations] RBRACES
                | LBRACES [EnumConstantList] [,] [EnumBodyDeclarations] RBRACES
                | LBRACES [EnumConstantList] [,] [EnumBodyDeclarations] RBRACES
                | LBRACES [EnumConstantList] [,] [EnumBodyDeclarations] RBRACES
                '''

def p_EnumConstantList(p):
    '''EnumConstantList : EnumConstant COMMA EnumConstantList
                        | EnumConstant'''

#TODO: CHeck
def p_EnumConstant(p):
    '''EnumConstant : EnumConstantModifier Identifier LPAREN ArgumentList RPAREN ClassBody
                    | EnumConstantModifier Identifier LPAREN ArgumentList RPAREN
                    | EnumConstantModifier Identifier LPAREN RPAREN ClassBody
                    | EnumConstantModifier Identifier LPAREN RPAREN
                    | EnumConstantModifier Identifier ClassBody
                    | Identifier LPAREN ArgumentList RPAREN ClassBody
                    | Identifier LPAREN RPAREN ClassBody
                    | Identifier ClassBody
                    | Identifier LPAREN ArgumentList RPAREN
                    | Identifier LPAREN RPAREN
                    | EnumConstantModifier Identifier
                    | Identifier '''


def p_EnumConstantModifier(p):
    '''EnumConstantModifier : Annotation'''


def p_EnumBodyDeclarations(p):
    '''EnumBodyDeclarations : SEMICOLON MultClassBodyDeclaration
                            | SEMICOLON '''

def p_InterfaceDeclaration(p):
    '''InterfaceDeclaration : NormalInterfaceDeclaration
                            | AnnotationTypeDeclaration'''

def p_NormalInterfaceDeclaration(p):
    '''NormalInterfaceDeclaration : MultInterfaceModifier interface Identifier TypeParameters ExtendsInterfaces InterfaceBody
                                  | interface Identifier TypeParameters ExtendsInterfaces InterfaceBody
                                  | MultInterfaceModifier interface Identifier ExtendsInterfaces InterfaceBody
                                  | MultInterfaceModifier interface Identifier TypeParameters InterfaceBody
                                  | MultInterfaceModifier interface Identifier InterfaceBody
                                  | interface Identifier ExtendsInterfaces InterfaceBody
                                  | interface Identifier TypeParameters InterfaceBody
                                  | interface Identifier InterfaceBody '''

def p_MultInterfaceModifier(p):
    '''MultInterfaceModifier : InterfaceModifier MultInterfaceModifier
                                      | InterfaceModifier'''

def p_InterfaceModifier(p):
    '''InterfaceModifier : InterfaceModifier1
                         | InterfaceModifier2'''

def p_InterfaceModifier1(p):
    '''InterfaceModifier1 : Annotation
                          | PUBLIC
                          | PROTECTED
                          | PRIVATE '''

def p_InterfaceModifier2(p):
    '''InterfaceModifier2 : ABSTRACT
                          | STATIC
                          | STRICTFP '''

def p_ExtendsInterfaces(p):
    '''ExtendsInterfaces : EXTENDS InterfaceTypeList '''

def p_InterfaceBody(p):
    '''InterfaceBody : LBRACES MultInterfaceMemberDeclaration RBRACES
                     | LBRACES RBRACES'''

def p_InterfaceMemberDeclaration(p):
    '''InterfaceMemberDeclaration : ConstantDeclaration
                                  | InterfaceMethodDeclaration
                                  | ClassDeclaration
                                  | InterfaceDeclaration
                                  | SEMICOLON'''

def p_MultInterfaceMemberDeclaration(p):
    '''MultInterfaceMemberDeclaration : InterfaceMemberDeclaration MultInterfaceMemberDeclaration
                                      | InterfaceMemberDeclaration'''


def p_ConstantDeclaration(p):
    '''ConstantDeclaration : MultConstantModifier UnannType VariableDeclaratorList SEMICOLON
                           | UnannType VariableDeclaratorList SEMICOLON '''

def p_MultConstantModifier(p):
    '''MultConstantModifier : ConstantModifier MultConstantModifier
                            | ConstantModifier '''

def p_ConstantModifier(p):
    '''ConstantModifier : ConstantModifier1
                        | ConstantModifier2'''

def p_ConstantModifier1(p):
    '''ConstantModifier1 : Annotation
                         | PUBLIC '''

def p_ConstantModifier2(p):
    '''ConstantModifier2 : STATIC
                         | FINAL '''

def p_InterfaceMethodDeclaration(p):
    '''InterfaceMethodDeclaration : MultInterfaceMethodModifier MethodHeader MethodBody
                                  | MethodHeader MethodBody '''

def p_MultInterfaceMethodModifier(p):
    '''MultInterfaceMethodModifier : InterfaceMethodModifier MultInterfaceMethodModifier
                            | InterfaceMethodModifier '''

def p_InterfaceMethodModifier(p):
    '''InterfaceMethodModifier : InterfaceMethodModifier1
                               | InterfaceMethodModifier2'''

def p_InterfaceMethodModifier1(p):
    '''InterfaceMethodModifier1 : Annotation
                                | PUBLIC '''

def p_InterfaceMethodModifier2(p):
    '''InterfaceMethodModifier2 : ABSTRACT
                                | DEFAULT
                                | STATIC
                                | STRICTFP'''

def p_AnnotationTypeDeclaration(p):
    '''AnnotationTypeDeclaration : MultInterfaceModifier AT INTERFACE Identifier AnnotationTypeBody
                                 | AT INTERFACE Identifier AnnotationTypeBody'''

def p_AnnotationTypeBody(p):
    '''AnnotationTypeBody : LBRACES MultAnnotationTypeMemberDeclaration RBRACES
                          | LBRACES RBRACES'''

def p_MultAnnotationTypeMemberDeclaration(p):
    '''MultAnnotationTypeMemberDeclaration : AnnotationTypeMemberDeclaration MultAnnotationTypeMemberDeclaration
                                           | AnnotationTypeMemberDeclaration'''

def p_AnnotationTypeMemberDeclaration(p):
    '''AnnotationTypeMemberDeclaration : AnnotationTypeElementDeclaration
                                       | ConstantDeclaration
                                       | ClassDeclaration
                                       | InterfaceDeclaration
                                       | SEMICOLON '''

def p_AnnotationTypeElementDeclaration(p):
    '''AnnotationTypeElementDeclaration : AnnotationTypeElementModifier UnannType Identifier LPAREN RPAREN Dims DefaultValue ;
                                        | UnannType Identifier LPAREN RPAREN Dims DefaultValue ;
                                        | AnnotationTypeElementModifier UnannType Identifier LPAREN RPAREN DefaultValue ;
                                        | AnnotationTypeElementModifier UnannType Identifier LPAREN RPAREN Dims ;
                                        | AnnotationTypeElementModifier UnannType Identifier LPAREN RPAREN ;
                                        | UnannType Identifier LPAREN RPAREN DefaultValue ;
                                        | UnannType Identifier LPAREN RPAREN Dims ;
                                        | UnannType Identifier LPAREN RPAREN ;
    '''


def p_MultAnnotationTypeElementModifier(p):
    '''MultAnnotationTypeElementModifier : AnnotationTypeElementModifier MultAnnotationTypeElementModifier
                      | AnnotationTypeElementModifier'''

def p_AnnotationTypeElementModifier(p):
    '''AnnotationTypeElementModifier : AnnotationTypeElementModifier1
                                     | AnnotationTypeElementModifier2'''

def p_AnnotationTypeElementModifier1(p):
    '''AnnotationTypeElementModifier1 : Annotation
                                      | PUBLIC'''

def p_AnnotationTypeElementModifier2(p):
    '''AnnotationTypeElementModifier2 : ABSTRACT'''

def p_DefaultValue(p):
    '''DefaultValue :  DEFAULT ElementValue'''

def p_Annotation(p):
    '''Annotation : NormalAnnotation
                  | MarkerAnnotation
                  | SingleElementAnnotation'''
def p_NormalAnnotation(p):
    '''NormalAnnotation : AT TypeName LPAREN [ElementValuePairList] RPAREN
                        | AT TypeName LPAREN RPAREN '''

def p_ElementValuePairList(p):
    '''ElementValuePairList : ElementValuePair COMMA ElementValuePairList
                            | ElementValuePair '''

def p_ElementValuePair(p):
    '''ElementValuePair : Identifier EQUAL ElementValue'''

def p_ElementValue(p):
    '''ElementValue : ConditionalExpression
                    | ElementValueArrayInitializer
                    | Annotation '''

def p_ElementValueArrayInitializer(p):
    '''ElementValueArrayInitializer : LBRACES ElementValueList COMMA RBRACES
                                    | LBRACES ElementValueList RBRACES
                                    | LBRACES COMMA RBRACES
                                    | LBRACES RBRACES '''

def p_ElementValueList(p):
    '''ElementValueList : ElementValue COMMA ElementValueList
                        | ElementValue'''

def p_MarkerAnnotation(p):
    '''MarkerAnnotation : AT TypeName '''

def p_SingleElementAnnotation(p):
    '''SingleElementAnnotation :  AT TypeName LPAREN ElementValue RPAREN'''

################################################################################

def p_ArrayInitializer(p):
    '''ArrayInitializer : LBRACES VariableInitializerList COMMA RBRACES
                        | LBRACES VariableInitializerList RBRACES
                        | LBRACES COMMA RBRACES
                        | LBRACES RBRACES
                        '''


def p_VariableInitializerList(p):
    '''VariableInitializerList : VariableInitializer COMMA VariableInitializerList
                               | VariableInitializer'''

################################################################################

def p_Block(p):
    '''Block : LBRACES BlockStatements RBRACES
             | LBRACES RBRACES '''

def p_BlockStatements(p):
    '''BlockStatements : BlockStatement {BlockStatement} '''


def p_BlockStatement(p):
    '''BlockStatement : LocalVariableDeclarationStatement
                      | ClassDeclaration
                      | Statement'''

def p_LocalVariableDeclarationStatement(p):
    '''LocalVariableDeclarationStatement : LocalVariableDeclaration SEMICOLON '''

def p_LocalVariableDeclaration(p):
    '''LocalVariableDeclaration : MultVariableModifier UnannType VariableDeclaratorList
                                | UnannType VariableDeclaratorList '''

def p_Statement(p):
    '''Statement : StatementWithoutTrailingSubstatement
                 | LabeledStatement
                 | IfThenStatement
                 | IfThenElseStatement
                 | WhileStatement
                 | ForStatement'''

def p_StatementNoShortIf(p):
    '''StatementNoShortIf : StatementWithoutTrailingSubstatement
                          | LabeledStatementNoShortIf
                          | IfThenElseStatementNoShortIf
                          | WhileStatementNoShortIf
                          | ForStatementNoShortIf'''

def p_StatementWithoutTrailingSubstatement(p):
    '''StatementWithoutTrailingSubstatement : Block
                                            | EmptyStatement
                                            | ExpressionStatement
                                            | AssertStatement
                                            | SwitchStatement
                                            | DoStatement
                                            | BreakStatement
                                            | ContinueStatement
                                            | ReturnStatement
                                            | SynchronizedStatement
                                            | ThrowStatement
                                            | TryStatement'''

def p_EmptyStatement(p):
    '''EmptyStatement : SEMICOLON'''

def p_LabeledStatement(p):
    '''LabeledStatement : Identifier COLON Statement '''

def p_LabeledStatementNoShortIf(p):
    '''LabeledStatementNoShortIf : Identifier COLON StatementNoShortIf '''

def p_ExpressionStatement(p):
    '''ExpressionStatement : StatementExpression SEMICOLON '''

def p_StatementExpression(p):
    '''StatementExpression : Assignment
                           | PreIncrementExpression
                           | PreDecrementExpression
                           | PostIncrementExpression
                           | PostDecrementExpression
                           | MethodInvocation
                           | ClassInstanceCreationExpression'''

def p_IfThenStatement(p):
    '''IfThenStatement :  IF LPAREN Expression RPAREN Statement'''
    '''IfThenStatement :  IF LPAREN Expression RPAREN Statement'''

def p_IfThenElseStatement(p):
    '''IfThenElseStatement : IF LPAREN Expression RPAREN StatementNoShortIf ELSE Statement '''

def p_IfThenElseStatementNoShortIf(p):
    '''IfThenElseStatementNoShortIf : IF LPAREN Expression RPAREN StatementNoShortIf ELSE StatementNoShortIf '''


def p_AssertStatement(p):
    '''AssertStatement : ASSERT Expression SEMICOLON
                       | ASSERT Expression COLON Expression SEMICOLON'''

def p_SwitchStatement(p):
    '''SwitchStatement : switch LPAREN Expression RPAREN SwitchBlock '''

def p_SwitchBlock(p):
    '''SwitchBlock : LBRACES {SwitchBlockStatementGroup} {SwitchLabel} RBRACES
                   | LBRACES {SwitchBlockStatementGroup} {SwitchLabel} RBRACES

def p_IfThenElseStatement(p):
    '''IfThenElseStatement : IF LPAREN Expression RPAREN StatementNoShortIf ELSE Statement '''

def p_IfThenElseStatementNoShortIf(p):
    '''IfThenElseStatementNoShortIf : IF LPAREN Expression RPAREN StatementNoShortIf ELSE StatementNoShortIf '''


def p_AssertStatement(p):
    '''AssertStatement : ASSERT Expression SEMICOLON
                       | ASSERT Expression COLON Expression SEMICOLON'''

def p_SwitchStatement(p):
    '''SwitchStatement : switch LPAREN Expression RPAREN SwitchBlock '''

def p_SwitchBlock(p):
    '''SwitchBlock : LBRACES {SwitchBlockStatementGroup} {SwitchLabel} RBRACES
                   | LBRACES {SwitchBlockStatementGroup} {SwitchLabel} RBRACES
                   | LBRACES {SwitchBlockStatementGroup} {SwitchLabel} RBRACES
                   | LBRACES {SwitchBlockStatementGroup} {SwitchLabel} RBRACES
                   '''


def p_SwitchBlockStatementGroup(p):
    '''SwitchBlockStatementGroup :  MultSwitchLabel BlockStatements'''


def p_MultSwitchLabel(p):
    '''MultSwitchLabel : SwitchLabel MultSwitchLabel
                       | SwitchLabel'''

def p_SwitchLabel(p):
    '''SwitchLabel : CASE ConstantExpression DOT
                   | CASE EnumConstantName DOT
                   | DEFAULT DOT'''

def p_EnumConstantName(p):
    '''EnumConstantName : Identifier'''


def p_WhileStatement(p):
    '''WhileStatement : WHILE LPAREN Expression RPAREN Statement '''

def p_WhileStatementNoShortIf(p):
    '''WhileStatementNoShortIf :  WHILE LPAREN Expression RPAREN StatementNoShortIf'''

def p_DoStatement(p):
    '''DoStatement :  DO Statement WHILE LPAREN Expression RPAREN SEMICOLON'''

def p_ForStatement(p):
    '''ForStatement : BasicForStatement
                    | EnhancedForStatement'''

def p_ForStatementNoShortIf(p):
    '''ForStatementNoShortIf : BasicForStatementNoShortIf
                             | EnhancedForStatementNoShortIf '''

def p_BasicForStatement(p):
    '''BasicForStatement : FOR LPAREN ForInit SEMICOLON Expression SEMICOLON ForUpdate RPAREN Statement
                         | FOR LPAREN SEMICOLON Expression SEMICOLON ForUpdate RPAREN Statement
                         | FOR LPAREN ForInit SEMICOLON SEMICOLON ForUpdate RPAREN Statement
                         | FOR LPAREN ForInit SEMICOLON Expression SEMICOLON ForUpdate RPAREN Statement
                         | FOR LPAREN ForInit SEMICOLON Expression SEMICOLON RPAREN Statement
                         | FOR LPAREN SEMICOLON SEMICOLON ForUpdate RPAREN Statement
                         | FOR LPAREN SEMICOLON Expression SEMICOLON RPAREN Statement
                         | FOR LPAREN ForInit SEMICOLON SEMICOLON RPAREN Statement '''

def p_BasicForStatementNoShortIf(p):
    '''BasicForStatementNoShortIf : FOR LPAREN ForInit SEMICOLON Expression SEMICOLON ForUpdate RPAREN StatementNoShortIf
                                  | FOR LPAREN SEMICOLON Expression SEMICOLON ForUpdate RPAREN StatementNoShortIf
                                  | FOR LPAREN ForInit SEMICOLON SEMICOLON ForUpdate RPAREN StatementNoShortIf
                                  | FOR LPAREN ForInit SEMICOLON Expression SEMICOLON ForUpdate RPAREN StatementNoShortIf
                                  | FOR LPAREN ForInit SEMICOLON Expression SEMICOLON RPAREN StatementNoShortIf
                                  | FOR LPAREN SEMICOLON SEMICOLON ForUpdate RPAREN StatementNoShortIf
                                  | FOR LPAREN SEMICOLON Expression SEMICOLON RPAREN StatementNoShortIf
                                  | FOR LPAREN ForInit SEMICOLON SEMICOLON RPAREN StatementNoShortIf '''

def p_ForInit(p):
    '''ForInit : StatementExpressionList
               | LocalVariableDeclaration'''

def p_ForUpdate(p):
    '''ForUpdate : StatementExpressionList'''

def p_StatementExpressionList(p):
    '''StatementExpressionList : StatementExpression COMMA StatementExpressionList
                               | StatementExpression'''

def p_EnhancedForStatement(p):
    '''EnhancedForStatement : for LPAREN MultVariableModifier UnannType VariableDeclaratorId COLON Expression RPAREN Statement
                            | for LPAREN UnannType VariableDeclaratorId COLON Expression RPAREN Statement '''


def p_EnhancedForStatementNoShortIf(p):
    '''EnhancedForStatementNoShortIf : for LPAREN MultVariableModifier UnannType VariableDeclaratorId COLON Expression RPAREN StatementNoShortIf
                                     | for LPAREN UnannType VariableDeclaratorId COLON Expression RPAREN StatementNoShortIf '''

def p_BreakStatement(p):
    '''BreakStatement : BREAK Identifier SEMICOLON
                      | BREAK SEMICOLON'''

def p_ContinueStatement(p):
    '''ContinueStatement : CONTINUE Identifier SEMICOLON
                         | CONTINUE SEMICOLON '''

def p_ReturnStatement(p):
    '''ReturnStatement : RETURN Expression SEMICOLON
                       | RETURN SEMICOLON'''

def p_ThrowStatement(p):
    '''ThrowStatement : THROW Expression SEMICOLON '''

def p_SynchronizedStatement(p):
    '''SynchronizedStatement : SYNCHRONIZED LPAREN Expression t_RPAREN Block '''

def p_TryStatement(p):
    '''TryStatement : try Block Catches
                    | try Block [Catches] Finally
                    | TryWithResourcesStatement '''

def p_Catches(p):
    '''Catches : MultCatchClause'''

def p_MultCatchClause(p):
    '''MultCatchClause : CatchClause MultCatchClause
                       | CatchClause'''

def p_CatchClause(p):
    '''CatchClause : CATCH LPAREN CatchFormalParameter RPAREN Block '''

def p_CatchFormalParameter(p):
    '''CatchFormalParameter : MultVariableModifier CatchType VariableDeclaratorId
                            | CatchType VariableDeclaratorId '''

def p_CatchType(p):
    '''CatchType : UnannClassType BOOLEANOR MultCatchType1
                 | UnannClassType '''

def p_MultCatchType1(p):
    '''MultCatchType : BOOLEANOR CatchType MultCatchType
                     | empty'''

def p_Finally(p):
    '''Finally : FINALLY Block '''

def p_TryWithResourcesStatement(p):
    '''TryWithResourcesStatement : TRY ResourceSpecification Block Catches Finally
                                 | TRY ResourceSpecification Block Catches
                                 | TRY ResourceSpecification Block Finally
                                 | TRY ResourceSpecification Block '''

def p_ResourceSpecification(p):
    '''ResourceSpecification : LPAREN ResourceList SEMICOLON RPAREN
                             | LPAREN ResourceList RPAREN '''

def p_ResourceList(p):
    '''ResourceList : Resource SEMICOLON ResourceList
                    | Resource'''

def p_Resource(p):
    '''Resource : MultVariableModifier UnannType VariableDeclaratorId EQUAL Expression
                | UnannType VariableDeclaratorId EQUAL Expression '''

################################################################################

def p_Primary(p):
    '''Primary : PrimaryNoNewArray
               | ArrayCreationExpression '''

def p_PrimaryNoNewArray(p):
    '''PrimaryNoNewArray : Literal
                         | ClassLiteral
                         | THIS
                         | TypeName DOT THIS
                         | LPAREN Expression RPAREN
                         | ClassInstanceCreationExpression
                         | FieldAccess
                         | ArrayAccess
                         | MethodInvocation
                         | MethodReference'''

def p_ClassLiteral(p):
    '''ClassLiteral : TypeName Brackets DOT CLASS
                    | NumericType Brackets DOT CLASS
                    | BOOLEAN Brackets DOT CLASS
                    | VOID DOT CLASS'''

def p_ClassInstanceCreationExpression(p):
    '''ClassInstanceCreationExpression :  | UnqualifiedClassInstanceCreationExpression
                                       | ExpressionName DOT UnqualifiedClassInstanceCreationExpression
                                       | Primary DOT UnqualifiedClassInstanceCreationExpression'''

def p_UnqualifiedClassInstanceCreationExpression(p):
    '''UnqualifiedClassInstanceCreationExpression : NEW TypeArguments ClassOrInterfaceTypeToInstantiate LPAREN ArgumentList RPAREN ClassBody
                                                  | NEW ClassOrInterfaceTypeToInstantiate LPAREN ArgumentList RPAREN ClassBody
                                                  | NEW TypeArguments ClassOrInterfaceTypeToInstantiate LPAREN RPAREN ClassBody
                                                  | NEW TypeArguments ClassOrInterfaceTypeToInstantiate LPAREN ArgumentList RPAREN
                                                  | NEW TypeArguments ClassOrInterfaceTypeToInstantiate LPAREN RPAREN
                                                  | NEW ClassOrInterfaceTypeToInstantiate LPAREN ArgumentList RPAREN
                                                  | NEW ClassOrInterfaceTypeToInstantiate LPAREN RPAREN ClassBody
                                                  | NEW ClassOrInterfaceTypeToInstantiate LPAREN RPAREN '''

def p_ClassOrInterfaceTypeToInstantiate(p):
    '''ClassOrInterfaceTypeToInstantiate : MultAnnotation Identifier ClassOrInterfaceTypeToInstantiate1 TypeArgumentsOrDiamond
                                         | Identifier ClassOrInterfaceTypeToInstantiate1 TypeArgumentsOrDiamond
                                         | MultAnnotation Identifier TypeArgumentsOrDiamond
                                         | MultAnnotation Identifier ClassOrInterfaceTypeToInstantiate1
                                         | MultAnnotation Identifier
                                         | Identifier TypeArgumentsOrDiamond
                                         | Identifier ClassOrInterfaceTypeToInstantiate1
                                         | Identifier '''

def p_ClassOrInterfaceTypeToInstantiate1(p):
    '''ClassOrInterfaceTypeToInstantiate1 : DOT MultAnnotation Identifier ClassOrInterfaceTypeToInstantiate1
                                          : DOT Identifier ClassOrInterfaceTypeToInstantiate1
                                          | empty '''

def p_TypeArgumentsOrDiamond(p):
    '''TypeArgumentsOrDiamond : TypeArguments
                              | LESSTHAN GREATERTHAN'''

def p_FieldAccess(p):
    '''FieldAccess : Primary DOT Identifier
                   | SUPER DOT Identifier
                   | TypeName DOT SUPER DOT Identifier'''

def p_ArrayAccess(p):
    '''ArrayAccess : ExpressionName LBRACKETS Expression RBRACKETS
                   | PrimaryNoNewArray LBRACKETS Expression RBRACKETS '''

def p_MethodInvocation(p):
    '''MethodInvocation :
                        | MethodName LPAREN ArgumentList RPAREN
                        | MethodName LPAREN RPAREN
                        | TypeName DOT TypeArguments Identifier LPAREN ArgumentList RPAREN
                        | TypeName DOT Identifier LPAREN ArgumentList RPAREN
                        | TypeName DOT TypeArguments Identifier LPAREN RPAREN
                        | TypeName DOT Identifier LPAREN RPAREN
                        | ExpressionName DOT TypeArguments Identifier LPAREN ArgumentList RPAREN
                        | ExpressionName DOT TypeArguments Identifier LPAREN RPAREN
                        | ExpressionName DOT Identifier LPAREN ArgumentList RPAREN
                        | ExpressionName DOT Identifier LPAREN RPAREN
                        | Primary DOT TypeArguments Identifier LPAREN ArgumentList RPAREN
                        | Primary DOT Identifier LPAREN ArgumentList RPAREN
                        | Primary DOT TypeArguments Identifier LPAREN RPAREN
                        | Primary DOT Identifier LPAREN RPAREN
                        | SUPER DOT TypeArguments Identifier LPAREN ArgumentList RPAREN
                        | SUPER DOT Identifier LPAREN ArgumentList RPAREN
                        | SUPER DOT TypeArguments Identifier LPAREN RPAREN
                        | SUPER DOT Identifier LPAREN RPAREN
                        | TypeName DOT SUPER DOT TypeArguments Identifier LPAREN ArgumentList RPAREN
                        | TypeName DOT SUPER DOT Identifier LPAREN ArgumentList RPAREN
                        | TypeName DOT SUPER DOT TypeArguments Identifier LPAREN RPAREN
                        | TypeName DOT SUPER DOT Identifier LPAREN RPAREN '''

def p_ArgumentList(p):
    '''ArgumentList : Expression COMMA ArgumentList
                    | Expression'''

def p_MethodReference(p):
    '''MethodReference : ExpressionName COLON COLON TypeArguments Identifier
                       | ExpressionName COLON COLON Identifier
                       | ReferenceType COLON COLON TypeArguments Identifier
                       | ReferenceType COLON COLON Identifier
                       | Primary COLON COLON TypeArguments Identifier
                       | Primary COLON COLON Identifier
                       | SUPER COLON COLON TypeArguments Identifier
                       | SUPER COLON COLON Identifier
                       | TypeName DOT SUPER COLON COLON TypeArguments Identifier
                       | TypeName DOT SUPER COLON COLON Identifier
                       | ClassType COLON COLON TypeArguments NEW
                       | ClassType COLON COLON NEW
                       | ArrayType COLON COLON NEW '''

def p_ArrayCreationExpression(p):
    '''ArrayCreationExpression : NEW PrimitiveType DimExprs Dims
                               | NEW PrimitiveType DimExprs
                               | NEW ClassOrInterfaceType DimExprs Dims
                               | NEW ClassOrInterfaceType DimExprs
                               | NEW PrimitiveType Dims ArrayInitializer
                               | NEW ClassOrInterfaceType Dims ArrayInitializer'''

def p_DimExprs(p):
    '''DimExprs : DimExpr DimExprs
                | DimExpr'''

def p_DimExpr(p):
    '''DimExpr : MultAnnotation LBRACKETS Expression RBRACKETS
               | LBRACKETS Expression RBRACKETS'''

def p_Expression(p):
    '''Expression : LambdaExpression
                  | AssignmentExpression '''

def p_LambdaExpression(p):
    '''LambdaExpression : LambdaParameters ARROW LambdaBody '''

def p_LambdaParameters(p):
    '''LambdaParameters : Identifier
                        | LPAREN FormalParameterList RPAREN
                        | LPAREN RPAREN
                        | LPAREN InferredFormalParameterList RPAREN '''


def p_InferredFormalParameterList(p):
    '''InferredFormalParameterList : Identifier COMMA InferredFormalParameterList
                                   | Identifier'''


def p_LambdaBody(p):
    '''LambdaBody : Expression
                  | Block '''

def p_AssignmentExpression(p):
    '''AssignmentExpression : ConditionalExpression
                            | Assignment '''

def p_Assignment(p):
    '''Assignment : LeftHandSide AssignmentOperator Expression'''

def p_LeftHandSide(p):
    '''LeftHandSide : ExpressionName
                    | FieldAccess
                    | ArrayAccess '''

def p_AssignmentOperator(p):
    '''AssignmentOperator : EQUAL
                          | MULTIPLYEQUALS
                          | DIVIDEEQUALS
                          | MODULOEQUALS
                          | PLUSEQUALS
                          | MINUSEQUALS
                          | LEFTSHIFTEQUALS
                          | RIGHTSHIFTEQUALS
                          | URIGHTSHIFTEQUALS
                          | ANDEQUALS
                          | XOREQUALS
                          | OREQUALS'''

def p_ConditionalExpression(p):
    '''ConditionalExpression : ConditionalOrExpression
                             | ConditionalOrExpression QUESTIONMARK Expression COLON ConditionalExpression
                             | ConditionalOrExpression QUESTIONMARK Expression COLON LambdaExpression'''


def p_ConditionalOrExpression(p):
    '''ConditionalOrExpression : ConditionalAndExpression
                               | ConditionalOrExpression || ConditionalAndExpression '''


def p_ConditionalAndExpression(p):
    '''ConditionalAndExpression : InclusiveOrExpression
                                | ConditionalAndExpression AND InclusiveOrExpression'''

def p_InclusiveOrExpression(p):
    '''InclusiveOrExpression : ExclusiveOrExpression
                             | InclusiveOrExpression BOOLEANOR ExclusiveOrExpression'''


def p_ExclusiveOrExpression(p):
    '''ExclusiveOrExpression : AndExpression
                             | ExclusiveOrExpression BOOLEANXOR AndExpression '''


def p_AndExpression(p):
    '''AndExpression : EqualityExpression
                     | AndExpression BOOLEANAND EqualityExpression '''


def p_EqualityExpression(p):
    '''EqualityExpression : RelationalExpression
                          | EqualityExpression EQUALS RelationalExpression
                          | EqualityExpression NOTEQUALS RelationalExpression '''


def p_RelationalExpression(p):
    '''RelationalExpression : ShiftExpression
                            | RelationalExpression LESSTHAN ShiftExpression
                            | RelationalExpression GREATERTHAN ShiftExpression
                            | RelationalExpression LESSTHANEQUAL ShiftExpression
                            | RelationalExpression GREATERTHANEQUAL ShiftExpression
                            | RelationalExpression INSTANCEOF ReferenceType'''


def p_ShiftExpression(p):
    '''ShiftExpression : AdditiveExpression
                       | ShiftExpression LEFTSHIFT AdditiveExpression
                       | ShiftExpression RIGHTSHIFT AdditiveExpression
                       | ShiftExpression URIGHTSHIFT AdditiveExpression '''

def p_AdditiveExpression(p):
    '''AdditiveExpression : MultiplicativeExpression
                          | AdditiveExpression PLUS MultiplicativeExpression
                          | AdditiveExpression MINUS MultiplicativeExpression '''

def p_MultiplicativeExpression(p):
    '''MultiplicativeExpression : UnaryExpression
                                | MultiplicativeExpression MULTIPLY UnaryExpression
                                | MultiplicativeExpression DIVIDE UnaryExpression
                                | MultiplicativeExpression MODULO UnaryExpression '''

def p_UnaryExpression(p):
    '''UnaryExpression : PreIncrementExpression
                       | PreDecrementExpression
                       | PLUS UnaryExpression
                       | MINUS UnaryExpression
                       | UnaryExpressionNotPlusMinus '''

def p_PreIncrementExpression(p):
    '''PreIncrementExpression : PLUSPLUS UnaryExpression '''

def p_PreDecrementExpression(p):
    '''PreDecrementExpression : MINUSMINUS UnaryExpression '''


def p_UnaryExpressionNotPlusMinus(p):
    '''UnaryExpressionNotPlusMinus : PostfixExpression
                                   | TILDA UnaryExpression
                                   | BOOLEANNOT UnaryExpression
                                   | CastExpression '''

def p_PostfixExpression(p):
    '''PostfixExpression :  Primary
                         | ExpressionName
                         | PostIncrementExpression
                         | PostDecrementExpression'''

def p_PostIncrementExpression(p):
    '''PostIncrementExpression : PostfixExpression PLUSPLUS'''

def p_PostDecrementExpression(p):
    '''PostDecrementExpression : PostfixExpression MINUSMINUS'''

def p_CastExpression(p):
    '''CastExpression : LPAREN PrimitiveType RPAREN UnaryExpression
                      | LPAREN ReferenceType AdditionalBound RPAREN UnaryExpressionNotPlusMinus
                      | LPAREN ReferenceType RPAREN UnaryExpressionNotPlusMinus
                      | LPAREN ReferenceType AdditionalBound RPAREN LambdaExpression
                      | LPAREN ReferenceType RPAREN LambdaExpression '''


def p_ConstantExpression(p):
    '''ConstantExpression : Expression '''

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





# Build the parser
parser = yacc.yacc()

