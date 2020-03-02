#JAVA_PARSER_Complier_Project

import re
import yacc
import json 
import sys, os
import traceback
from lexer import lexer,tokens,keywords
from types import SimpleNamespace

BOOLEAN_TYPES = ['boolean']
INTEGRAL_TYPES = ['byte', 'char', 'short', 'int', 'long']
FLOATING_POINT_TYPES = ['float', 'double']
STRING_TYPES = ['string']

NUMERIC_TYPES = BOOLEAN_TYPES + INTEGRAL_TYPES + FLOATING_POINT_TYPES

class STR:
    TYPE          = 'type'
    CLASS_TYPE        = 'class_type'
    IDENTIFIER_TYPE        = 'identifier_type'
    ANNOTATION_TYPE   = 'annotation_type'
    VALUE           = 'value'

    MODIFIERS         = 'modifiers'
    BLOCKS            = 'blocks'
    TYPE_PARAMS       = 'type_parameters'
    SUPER_CLASS       = 'super_class'
    SUPER_INTERFACES  = 'super_interface'
    CLASS_DECL        = 'class_declaration'
    INTERFACE_DECL    = 'interface_declaration'
    FIELD_DECL        = 'field_declaration'
    METHOD_DECL       = 'method_declaration'
    CLASS_BODY        = 'class_body'
    CLASS_DECL_TYPE   = 'class_decl_type'

    ANNOTATIONS       = 'annotations'
    ANNOT_ID          = 'annotation_identifier'
    NORMAL_ANNOT      = 'normal_annotation'
    MARKER_ANNOT      = 'marker_annotation'
    SINGLE_ELEM_ANNOT = 'single_element_annotation'
    ELEM_VALUE = 'element_value'
    ELEM_VALUE_PAIR = 'element_value_pair'

    THROWS ='throws'

################################################################################

class DeclarationError(Exception):
    pass

################################################################################

class SymbolTable():

    def __init__(self):
        #  self.block_type = scope_type            # Global / Class / Function / Block Scope
        self.__children = []
        self.__declarations = {}
        self.__parent = None 

    def printST(self):
        print(json.dumps(self.__declarations, indent=4))

    def setParent(self, parent):
        self.__parent = parent

    def getParent(self, parent):
        return self.__parent

    def lookupSymbol(self, symbol):
        if (self.__parent is None):
            return self.__declarations.get(symbol, None)
        else:
            return self.__parent.lookupSymbol(symbol)

    def addChildren(self, child):
        self.__children.append(child)

    def popChild(self, child):
        return self.__children.pop(0) if len(self.__children) > 0 else None

    #  obj = { 'type': str,  modifiers: 'list', value: }
    #TODO: Possibly fetch properties from obj and set them manually
    def insertSymbol(self, symbol, obj):
        if (symbol in self.__declarations.keys()):
            return False
        else:
            self.__declarations[str(symbol)] = obj
            return True

globalST = SymbolTable()
currentST = SymbolTable()

################################################################################

def logger(*args):
    func_name = traceback.extract_stack(None, 2)[-2][2]
    print(func_name + ":" + "\t\t")
    for i in args:
        print(json.dumps(i, indent=4))
    print()

def assertExpressionType(allowedTypes, *args):
    if (allowedTypes is None and len(args) == 1):
        return args[0][STR.TYPE]
    elif ((allowedTypes is None and len(args) == 2)):
        sameType = True 
        for i in range(1, len(args)):
            if (args[i-1][STR.TYPE] != args[i][STR.TYPE]):
                sameType = False 
                raise TypeError("Expected %s, but received %s" % (args[i-1][STR.TYPE], args[i][STR.TYPE]))
    elif (allowedTypes is None):
        raise TypeError("Expected one from %s, but received %s" % (allowedTypes, args))

    if ('string' in allowedTypes):
        isString = False
        for i in args:
            #Check if string types are present in args array
            isString |= True if i[STR.TYPE] == 'string' else False

        if (isString):
            for i in args:
                if (i[STR.TYPE] != 'string'):
                    raise TypeError("Expected all args to be strings, but received another arg='%s'" % (i[STR.TYPE]))

    retIndex = -1
    for i in args:
        iType = i[STR.TYPE]

        if iType not in allowedTypes:
            raise TypeError("Expected %s, but received %s" % (allowedTypes, iType))
        else:
            # The allowedTypes array is supposed to besorted in increasing priority
            retIndex = max(retIndex, allowedTypes.index(iType))

    return allowedTypes[retIndex]

#  def copyObjProperties(dest, src):

################################################################################

def p_start(p):
    '''start : CompilationUnit '''
    global globalST, currentST, STR
    
    logger(currentST.printST())

def p_Modifier(p):
    ''' Modifier : PUBLIC
                 | PROTECTED
                 | PRIVATE
                 | ABSTRACT
                 | STATIC
                 | FINAL
                 | STRICTFP
                 | TRANSIENT
                 | VOLATILE
                 | SYNCHRONIZED
                 | NATIVE
                 | DEFAULT'''
    global globalST, currentST, STR

    p[0] = {
        STR.MODIFIERS: [(p[1])]
    }

def p_ModifierList(p):
    ''' ModifierList : MultModifier
                   | MultAnnotation MultModifier
                   | MultAnnotation
                   '''
    global globalST, currentST, STR

    p[0] = {
        STR.MODIFIERS: list(),
        STR.ANNOTATIONS: list(),
    }

    for i in range(1, len(p)):
        logger(p[i])
        if (p[i] is not None) and STR.MODIFIERS in p[i]:
            p[0][STR.MODIFIERS] += p[i].get(STR.MODIFIERS, [])

        if (p[i] is not None) and STR.ANNOTATIONS in p[i]:
            p[0][STR.ANNOTATIONS] += p[i].get(STR.ANNOTATIONS, [])
        
def p_MultModifier(p):
    ''' MultModifier : Modifier MultModifier
                     | Modifier '''
    global globalST, currentST, STR

    mods = p[1].get(STR.MODIFIERS, [])
    p[0] = {
        STR.MODIFIERS: mods if len(p) == 2 else mods + p[2].get(STR.MODIFIERS, [])
    }

# TODO: Support FOR unicode needs to be added
def p_Identifier(p):
    '''Identifier : IDENTIFIER'''
    global globalST, currentST, STR

    p[0] = {
        STR.IDENTIFIER_TYPE: p[0][STR.IDENTIFIER_TYPE] if p[0] is not None else None,
        "ID": [p[1]]
    }

def p_Literal(p):
    '''Literal : DECIMALINT
               | DECIMALFLOATINGLIT
               | HEXINT
               | HEXFLOATINGLIT
               | OCTALINT
               | BOOLEANLIT
               | CHARLIT
               | STRINGLIT
               | NULLLIT
               | BINARYINT'''
    global globalST, currentST, STR

    lexeme = str(p.slice[1].value)
    logger(lexeme)
    # Should we take the type from the declaration?
    long_or_int = 'long' if re.search('(l|L)', lexeme) is not None else 'int'
    float_or_double = 'double' if re.search('(d|D)', lexeme) is not None else 'float'

    lit_types = {
            'DECIMALINT': long_or_int,
            'DECIMALFLOATINGLIT': float_or_double,
            'HEXINT': long_or_int,
            'HEXFLOATINGLIT': float_or_double,
            'OCTALINT': long_or_int,
            'BOOLEANLIT': 'boolean',
            'CHARLIT': 'char',
            'STRINGLIT': 'string',
            'NULLLIT': 'null',
            'BINARYINT': long_or_int
        }
  
    p[0] = {
        STR.TYPE: lit_types[p.slice[1].type],
        STR.VALUE: p[1],
    }
    logger(p[0])

def p_Type(p):
    '''Type : PrimitiveType
            | ReferenceType'''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: p[1][STR.TYPE]
    }

def p_PrimitiveType(p):
    '''PrimitiveType : MultAnnotation NumericType
                     | MultAnnotation BooleanType
                     | NumericType
                     | BooleanType'''
    global globalST, currentST, STR

    logger(p[1:])
    type_index = len(p) - 1
    p[0] = {
        STR.TYPE: p[type_index][STR.TYPE],
        STR.ANNOTATIONS: list() if len(p) == 2 else p[1][STR.ANNOTATIONS]
    }

def p_BooleanType(p):
    '''BooleanType : BOOLEAN'''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: p[1]
    }

def p_NumericType(p):
    '''NumericType : IntegralType
                   | FloatingPointType'''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: p[1][STR.TYPE]
    }

def p_IntegralType(p):
    '''IntegralType : BYTE
                    | SHORT
                    | INT
                    | LONG
                    | CHAR'''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: p[1]
    }

def p_FloatingPointType(p):
    '''FloatingPointType : FLOAT
                         | DOUBLE'''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: p[1]
    }

def p_ReferenceType(p):
    '''ReferenceType : ClassOrInterfaceType
                     | ArrayType'''
    global globalST, currentST, STR

def p_ClassOrInterfaceType(p):
    '''ClassOrInterfaceType : ClassType'''
    global globalST, currentST, STR

    p[0] = p[1]

def p_ClassType(p):
    '''ClassType : ClassOrInterfaceType DOT MultAnnotation Identifier TypeArguments
                 | ClassOrInterfaceType DOT MultAnnotation Identifier
                 | ClassOrInterfaceType DOT Identifier TypeArguments
                 | ClassOrInterfaceType DOT Identifier
                 | Identifier DOT MultAnnotation Identifier TypeArguments
                 | Identifier DOT MultAnnotation Identifier
                 | Identifier DOT Identifier TypeArguments
                 | TypeName
                 | MultAnnotation Identifier TypeArguments
                 | MultAnnotation Identifier
                 | Identifier TypeArguments
                 '''
    global globalST, currentST, STR

    logger(p[1:])

def p_MultAnnotation(p):
    '''MultAnnotation : Annotation MultAnnotation
                      | Annotation'''
    global globalST, currentST, STR

    mods = p[1].get(STR.ANNOTATIONS, [])
    p[0] = {
        STR.ANNOTATIONS: mods if len(p) == 2 else mods + p[2].get(STR.ANNOTATIONS, [])
    }

def p_ArrayType(p):
    '''ArrayType : PrimitiveType Dims
                 | ClassOrInterfaceType Dims'''
    global globalST, currentST, STR


def p_Dims(p):
    '''Dims : MultAnnotation LBRACKETS RBRACKETS Dims
            | MultAnnotation LBRACKETS RBRACKETS
            | LBRACKETS RBRACKETS Dims
            | LBRACKETS RBRACKETS'''
    global globalST, currentST, STR


def p_TypeArguments(p):
    '''TypeArguments : TYPE_ARG_BEGIN TypeArgumentList GREATERTHAN
                     | TYPE_ARG_BEGIN CommaSeparatedIdentifiers GREATERTHAN
                     | TYPE_ARG_BEGIN Identifier GREATERTHAN
                     | TYPE_ARG_BEGIN GREATERTHAN'''
    global globalST, currentST, STR


def p_TypeArgumentList(p):
    '''TypeArgumentList : TypeArgument COMMA TypeArgumentList
                        | TypeArgument'''
    global globalST, currentST, STR


def p_TypeArgument(p):
    '''TypeArgument : ReferenceType
                    | Wildcard'''
    global globalST, currentST, STR


def p_Wildcard(p):
    '''Wildcard : MultAnnotation QUESTIONMARK WildcardBounds
                | MultAnnotation QUESTIONMARK
                | QUESTIONMARK WildcardBounds
                | QUESTIONMARK'''
    global globalST, currentST, STR


def p_WildcardBounds(p):
    '''WildcardBounds : EXTENDS ReferenceType
                      | SUPER ReferenceType'''
    global globalST, currentST, STR


#This just points to DotSeparatedIdentifiers ( with atleast a dot) All TypeName -> Identifier replaced by Identifier
#TODO: Here the type name should be inferred using the dots, right?
def p_TypeName(p):
    '''TypeName : TypeName DOT Identifier
                | Identifier DOT Identifier'''
    global globalST, currentST, STR

    p[0] = {}
    if (p[1] is not None and STR.IDENTIFIER_TYPE in p[1]):
        p[0][STR.TYPE] = '.'.join([p[1][STR.IDENTIFIER_TYPE], p[3][STR.IDENTIFIER_TYPE]])
    elif (p[1] is not None and STR.TYPE in p[1]):
        p[0][STR.TYPE] = '.'.join([p[1][STR.TYPE], p[3][STR.IDENTIFIER_TYPE]])

    logger(p[0])

#Removing Empty Program in Java
def p_CompilationUnit(p):
    '''CompilationUnit : PackageDeclaration MultImportDeclaration MultTypeDeclaration
                       | PackageDeclaration MultImportDeclaration
                       | PackageDeclaration MultTypeDeclaration
                       | MultImportDeclaration MultTypeDeclaration
                       | MultTypeDeclaration
                       | PackageDeclaration
                       | MultImportDeclaration
                       '''
    global globalST, currentST, STR


def p_MultImportDeclaration(p):
    '''MultImportDeclaration : ImportDeclaration MultImportDeclaration
                             | ImportDeclaration'''
    global globalST, currentST, STR


def p_MultTypeDeclaration(p):
    '''MultTypeDeclaration : TypeDeclaration MultTypeDeclaration
                           | TypeDeclaration'''
    global globalST, currentST, STR


def p_PackageDeclaration(p):
    '''PackageDeclaration : MultAnnotation PACKAGE TypeName SEMICOLON
                          | PACKAGE TypeName SEMICOLON
                          | MultAnnotation PACKAGE Identifier SEMICOLON
                          | PACKAGE Identifier SEMICOLON'''
    global globalST, currentST, STR


def p_ImportDeclaration(p):
    '''ImportDeclaration : SingleTypeImportDeclaration
                         | TypeImportOnDemandDeclaration
                         | SingleStaticImportDeclaration
                         | StaticImportOnDemandDeclaration '''
    global globalST, currentST, STR


def p_SingleTypeImportDeclaration(p):
    '''SingleTypeImportDeclaration : IMPORT TypeName SEMICOLON 
                                   | IMPORT Identifier SEMICOLON '''
    global globalST, currentST, STR


def p_TypeImportOnDemandDeclaration(p):
    '''TypeImportOnDemandDeclaration : IMPORT TypeName DOT MULTIPLY SEMICOLON
                                     | IMPORT Identifier DOT MULTIPLY SEMICOLON'''
    global globalST, currentST, STR


def p_SingleStaticImportDeclaration(p):
    '''SingleStaticImportDeclaration : IMPORT STATIC TypeName DOT Identifier SEMICOLON 
                                     | IMPORT STATIC Identifier DOT Identifier SEMICOLON '''
    global globalST, currentST, STR


def p_StaticImportOnDemandDeclaration(p):
    '''StaticImportOnDemandDeclaration : IMPORT STATIC TypeName DOT MULTIPLY SEMICOLON
                                       | IMPORT STATIC Identifier DOT MULTIPLY SEMICOLON'''
    global globalST, currentST, STR


def p_TypeDeclaration(p):
    '''TypeDeclaration : ClassDeclaration
                       | InterfaceDeclaration
                       | SEMICOLON'''
    global globalST, currentST, STR

    logger(p[1:])
    #  if p[1] != ';':


def p_ClassDeclaration(p):
    '''ClassDeclaration : NormalClassDeclaration
                        | EnumDeclaration'''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = p[1]

def p_NormalClassDeclaration(p):
    '''NormalClassDeclaration : ModifierList CLASS Identifier TypeParameters Superclass Superinterfaces ClassBody
                              | ModifierList CLASS Identifier TypeParameters Superclass ClassBody
                              | ModifierList CLASS Identifier TypeParameters Superinterfaces ClassBody
                              | ModifierList CLASS Identifier Superclass Superinterfaces ClassBody
                              | ModifierList CLASS Identifier TypeParameters ClassBody
                              | ModifierList CLASS Identifier Superclass ClassBody
                              | ModifierList CLASS Identifier Superinterfaces ClassBody
                              | ModifierList CLASS Identifier ClassBody
                              | CLASS Identifier TypeParameters Superclass Superinterfaces ClassBody
                              | CLASS Identifier TypeParameters Superclass ClassBody
                              | CLASS Identifier TypeParameters Superinterfaces ClassBody
                              | CLASS Identifier Superclass Superinterfaces ClassBody
                              | CLASS Identifier TypeParameters ClassBody
                              | CLASS Identifier Superclass ClassBody
                              | CLASS Identifier Superinterfaces ClassBody
                              | CLASS Identifier ClassBody'''
    global globalST, currentST, STR

    logger(p[1:])
    modifiers = [] if p[1] == 'class' else p[1].get(STR.MODIFIERS, [])
    k = p[2:] if p[1] == 'class' else p[3:]
    
    classObj = {
        "ID"                 : k[0].get("ID")[0],
        STR.MODIFIERS        : modifiers,
        STR.TYPE_PARAMS      : None,
        STR.SUPER_CLASS      : None,
        STR.SUPER_INTERFACES : None,
        STR.CLASS_BODY       : None,
    }
    
    for i in k[0:]:
        if (STR.TYPE_PARAMS in i):
            classObj[STR.TYPE_PARAMS] = i[STR.TYPE_PARAMS]
            print(STR.TYPE_PARAMS)

        if (STR.SUPER_CLASS in i):
            classObj[STR.SUPER_CLASS] = i[STR.SUPER_CLASS]
            print(STR.SUPER_CLASS)

        if (STR.SUPER_INTERFACES in i):
            classObj[STR.SUPER_INTERFACES] = i[STR.SUPER_INTERFACES]
            print(STR.SUPER_INTERFACES)

        if (STR.CLASS_BODY in i):
            classObj[STR.CLASS_BODY] = i[STR.CLASS_BODY]

    logger(classObj)

def p_TypeParameters(p):
    '''TypeParameters : TYPE_ARG_BEGIN TypeParameterList GREATERTHAN
                      | TYPE_ARG_BEGIN CommaSeparatedIdentifiers GREATERTHAN
                      | TYPE_ARG_BEGIN Identifier GREATERTHAN'''
    global globalST, currentST, STR
    

def p_CommaSeparatedIdentifiers(p):
    ''' CommaSeparatedIdentifiers : Identifier COMMA CommaSeparatedIdentifiers
                                  | Identifier COMMA Identifier'''
    global globalST, currentST, STR


#Does not contain Identifier List
def p_TypeParameterList(p):
    '''TypeParameterList : TypeParameter COMMA TypeParameterList
                         | TypeParameter'''
    global globalST, currentST, STR

def p_TypeParameter(p):
    '''TypeParameter : MultAnnotation Identifier TypeBound
                     | MultAnnotation Identifier
                     | Identifier TypeBound
                     '''
    global globalST, currentST, STR


def p_TypeBound(p):
    '''TypeBound : EXTENDS ClassOrInterfaceType MultAdditionalBound
                 | EXTENDS ClassOrInterfaceType
                 | EXTENDS Identifier MultAdditionalBound
                 | EXTENDS Identifier
                 '''
    global globalST, currentST, STR


def p_MultAdditionalBound(p):
    ''' MultAdditionalBound : MultAdditionalBound AdditionalBound
                          | AdditionalBound '''
    global globalST, currentST, STR


def p_AdditionalBound(p):
    '''AdditionalBound : BOOLEANAND ClassType'''
    global globalST, currentST, STR


def p_Superclass(p):
    '''Superclass : EXTENDS ClassType
                  | EXTENDS Identifier'''
    global globalST, currentST, STR


def p_Superinterfaces(p):
    '''Superinterfaces : IMPLEMENTS InterfaceTypeList '''
    global globalST, currentST, STR


def p_InterfaceTypeList(p):
    '''InterfaceTypeList : ClassType COMMA InterfaceTypeList
                         | ClassType
                         | Identifier COMMA InterfaceTypeList
                         | Identifier '''
    global globalST, currentST, STR


def p_ClassBody(p):
    '''ClassBody : LBRACES MultClassBodyDeclaration RBRACES
                 | LBRACES RBRACES'''
    global globalST, currentST, STR
    
    logger(p[1:])
    p[0] = {
        STR.CLASS_BODY: p[2] if p[2] != '}' else {}
    }
    

def p_MultClassBodyDeclaration(p):
    '''MultClassBodyDeclaration : ClassBodyDeclaration MultClassBodyDeclaration
                                | ClassBodyDeclaration'''
    global globalST, currentST, STR

    logger(p[1:])
    class_decl = p[1].get(STR.CLASS_DECL, [])
    p[0] = {
        STR.CLASS_DECL: class_decl if len(p) == 2 else class_decl + p[2].get(STR.CLASS_DECL, [])
    }

def p_ClassBodyDeclaration(p):
    '''ClassBodyDeclaration : ClassMemberDeclaration
                            | InstanceInitializer
                            | StaticInitializer
                            | ConstructorDeclaration'''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.CLASS_DECL: p[1] if p[1] is not None else []
    }

def p_ClassMemberDeclaration(p):
    '''ClassMemberDeclaration : FieldDeclaration
                              | MethodDeclaration
                              | ClassDeclaration
                              | InterfaceDeclaration
                              | SEMICOLON '''
    global globalST, currentST, STR

    decls = [STR.FIELD_DECL, STR.METHOD_DECL, STR.CLASS_DECL, STR.INTERFACE_DECL]
    p[0] = None if p[1] == ';' else p[1]
    logger(p[0])

#Added Type <--> Identifier to remove ClassType->Identifier
def p_FieldDeclaration(p):
    '''FieldDeclaration : ModifierList Type VariableDeclaratorList SEMICOLON
                        | ModifierList Type Identifier SEMICOLON
                        | ModifierList Identifier VariableDeclaratorList SEMICOLON
                        | ModifierList Identifier Identifier SEMICOLON
                        | Type VariableDeclaratorList SEMICOLON
                        | Type Identifier SEMICOLON
                        | Identifier VariableDeclaratorList SEMICOLON
                        | Identifier Identifier SEMICOLON'''
    global globalST, currentST, STR

    modifiers = list() if len(p) == 4 else p[1][STR.MODIFIERS]
    annotations = list() if len(p) == 4 else p[1][STR.ANNOTATIONS]

    k = p[1:] if len(p) == 4 else p[2:]

    logger(p[0])
    # Here we pass a list because if we add another key "ID", then it becomes redundant
    p[0] = []

    if (k[1] is not None and k[1]["ID"] is not None):
        for ID in k[1]["ID"]:
            ID_obj = {
                    "ID"                : ID,
                    STR.ANNOTATIONS     : annotations,
                    STR.MODIFIERS       : modifiers,
                    STR.CLASS_DECL_TYPE : STR.FIELD_DECL
            }

            if k[0] is not None:
                if STR.IDENTIFIER_TYPE in k[0]:
                    ID_obj[STR.IDENTIFIER_TYPE] = k[0].get(STR.IDENTIFIER_TYPE) 

                if STR.TYPE in k[0]:
                    ID_obj[STR.IDENTIFIER_TYPE] = k[0].get(STR.TYPE) 

            p[0].append(ID_obj)

            if (not currentST.insertSymbol(ID, ID_obj)):
                raise DeclarationError("Variable %s has already been declared in current scope. " % (ID))
    
    logger(p[0])

def p_VariableDeclaratorList(p):
    '''VariableDeclaratorList : VariableDeclarator COMMA VariableDeclaratorList
                              | VariableDeclarator COMMA Identifier
                              | Identifier COMMA VariableDeclaratorList
                              | Identifier COMMA Identifier
                              | VariableDeclarator 
                              '''
    global globalST, currentST, STR

    logger(p[1:])
    if (len(p) == 2):
        p[0] = {
            "ID": p[1].get("ID", [])
        }
    else:
        logger(p[1].get("ID", []), p[3].get("ID", []))
        p[0] = {
            "ID": p[1].get("ID", []) + p[3].get("ID", [])
        }

def p_VariableDeclarator(p):
    '''VariableDeclarator : VariableDeclaratorId EQUAL VariableInitializer
                          | VariableDeclaratorId
                          | Identifier EQUAL VariableInitializer
                          '''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.IDENTIFIER_TYPE: p[1].get(STR.IDENTIFIER_TYPE, ""),
        "ID": p[1].get("ID", [])
    }

def p_VariableDeclaratorId(p):
    '''VariableDeclaratorId : Identifier Dims'''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.IDENTIFIER_TYPE: p[1][STR.IDENTIFIER_TYPE],
        "ID": p[1].get("ID", [])
    }


def p_VariableInitializer(p):
    '''VariableInitializer : Expression
                           | ArrayInitializer '''
    global globalST, currentST, STR


def p_MethodDeclaration(p):
    '''MethodDeclaration : ModifierList MethodHeader MethodBody
                         | MethodHeader MethodBody'''
    global globalST, currentST, STR

    modifiers = list() if len(p) == 3 else p[1][STR.MODIFIERS]
    annotations = list() if len(p) == 3 else p[1][STR.ANNOTATIONS]

    k = p[1:] if len(p) == 3 else p[2:]

    #  if (k[1] is not None and k[1]["ID"] is not None):
        #  for ID in k[1]["ID"]:
            #  ID_obj = {
                    #  "ID": ID,
                    #  STR.TYPE: k[0].get(STR.TYPE),
                    #  STR.ANNOTATIONS: annotations,
                    #  STR.MODIFIERS: modifiers 
            #  }
            #  p[0]["ID"].append(ID_obj)

            #  if (not currentST.insertSymbol(ID, ID_obj)):
                #  raise DeclarationError("Variable %s has already been declared in current scope. " % (ID))
    
    

#Added Result <--> Identifier to remove ClassType->Identifier (for Type->Identifier conflict)
def p_MethodHeader(p):
    '''MethodHeader : Type MethodDeclarator Throws
                    | Type MethodDeclarator
                    | VOID MethodDeclarator Throws
                    | VOID MethodDeclarator
                    | Identifier MethodDeclarator Throws
                    | Identifier MethodDeclarator
                    | TypeParameters MultAnnotation Type MethodDeclarator Throws
                    | TypeParameters MultAnnotation Type MethodDeclarator
                    | TypeParameters Type MethodDeclarator Throws
                    | TypeParameters Type MethodDeclarator
                    | TypeParameters MultAnnotation VOID MethodDeclarator Throws
                    | TypeParameters MultAnnotation VOID MethodDeclarator
                    | TypeParameters VOID MethodDeclarator Throws
                    | TypeParameters VOID MethodDeclarator
                    | TypeParameters MultAnnotation Identifier MethodDeclarator Throws
                    | TypeParameters MultAnnotation Identifier MethodDeclarator
                    | TypeParameters Identifier MethodDeclarator Throws
                    | TypeParameters Identifier MethodDeclarator
                    '''
    global globalST, currentST, STR
   
    logger(p[1:])
    methodObj = {
        "ID": None,
        "ReturnType": None
    }

    for i in range(len(p)):
        if (p[i] == 'void'):
            methodObj["ReturnType"] = 'void' 
            continue

        if (p[i] is not None):
            if STR.TYPE in p[i]:
                if (methodObj["ReturnType"] is not None):
                    methodObj["ReturnType"] = p[i].get(STR.TYPE, None)

            if STR.IDENTIFIER_TYPE in p[i]:
                if (methodObj["ReturnType"] is not None):
                    methodObj["ReturnType"] = p[i].get(STR.IDENTIFIER_TYPE)[0]

            if STR.ANNOTATIONS in p[i]:
                methodObj["Annotations"] = p[i].get(STR.ANNOTATIONS)

    logger(methodObj)

def p_MethodDeclarator(p):
    '''MethodDeclarator : Identifier LPAREN FormalParameterList RPAREN Dims
                        | Identifier LPAREN FormalParameterList RPAREN
                        | Identifier LPAREN RPAREN Dims
                        | Identifier LPAREN RPAREN'''
    global globalST, currentST, STR


def p_FormalParameterList(p):
    '''FormalParameterList : ReceiverParameter
                           | FormalParameters
                           | FormalParameters COMMA Identifier
                           | LastFormalParameter '''
    global globalST, currentST, STR


def p_FormalParameters(p):
    '''FormalParameters : FormalParameter COMMA FormalParameters
                        | FormalParameter
                        | LastFormalParameter '''
    global globalST, currentST, STR

#Added Type <--> Identifier to remove ClassType->Identifier
def p_FormalParameter(p):
    '''FormalParameter : ModifierList Type VariableDeclaratorId
                       | ModifierList Type Identifier 
                       | Type VariableDeclaratorId
                       | Type Identifier
                       | ModifierList Identifier VariableDeclaratorId
                       | ModifierList Identifier Identifier 
                       | Identifier VariableDeclaratorId
                       | Identifier Identifier '''
    global globalST, currentST, STR

#Added Type <--> Identifier to remove ClassType->Identifier
def p_LastFormalParameter(p):
    '''LastFormalParameter : ModifierList Type MultAnnotation ELLIPSIS VariableDeclaratorId
                           | ModifierList Type ELLIPSIS VariableDeclaratorId
                           | ModifierList Type MultAnnotation ELLIPSIS Identifier
                           | ModifierList Type ELLIPSIS Identifier 
                           | Type MultAnnotation ELLIPSIS VariableDeclaratorId
                           | Type ELLIPSIS VariableDeclaratorId
                           | Type MultAnnotation ELLIPSIS Identifier
                           | Type ELLIPSIS Identifier 
                           | ModifierList Identifier MultAnnotation ELLIPSIS VariableDeclaratorId
                           | ModifierList Identifier ELLIPSIS VariableDeclaratorId
                           | ModifierList Identifier MultAnnotation ELLIPSIS Identifier
                           | ModifierList Identifier ELLIPSIS Identifier 
                           | Identifier MultAnnotation ELLIPSIS VariableDeclaratorId
                           | Identifier ELLIPSIS VariableDeclaratorId
                           | Identifier MultAnnotation ELLIPSIS Identifier
                           | Identifier ELLIPSIS Identifier
                           '''
    global globalST, currentST, STR

#Added Type <--> Identifier to remove ClassType->Identifier
def p_ReceiverParameter(p):
    '''ReceiverParameter : MultAnnotation Type Identifier DOT THIS
                         | MultAnnotation Type THIS
                         | Type Identifier DOT THIS
                         | Type THIS
                         | MultAnnotation Identifier Identifier DOT THIS
                         | MultAnnotation Identifier THIS
                         | Identifier Identifier DOT THIS
                         | Identifier THIS'''
    global globalST, currentST, STR


def p_Throws(p):
    '''Throws : THROWS ExceptionTypeList
              | THROWS Identifier'''
    global globalST, currentST, STR


def p_ExceptionTypeList(p):
    '''ExceptionTypeList : ExceptionType COMMA ExceptionTypeList
                         | Identifier COMMA ExceptionTypeList
                         | Identifier COMMA Identifier
                         | ExceptionType '''
    global globalST, currentST, STR


def p_ExceptionType(p):
    '''ExceptionType : ClassType'''
    global globalST, currentST, STR
    p[0] = p[1]

def p_MethodBody(p):
    '''MethodBody : Block
                  | SEMICOLON'''
    global globalST, currentST, STR


def p_InstanceInitializer(p):
    '''InstanceInitializer :  Block'''
    global globalST, currentST, STR


def p_StaticInitializer(p):
    '''StaticInitializer : STATIC Block'''
    global globalST, currentST, STR


def p_ConstructorDeclaration(p):
    '''ConstructorDeclaration : ModifierList MethodDeclarator Throws ConstructorBody
                              | MethodDeclarator Throws ConstructorBody
                              | ModifierList MethodDeclarator ConstructorBody
                              | MethodDeclarator ConstructorBody'''
    global globalST, currentST, STR


def p_ConstructorBody(p):
    '''ConstructorBody : LBRACES ExplicitConstructorInvocation MultBlockStatement RBRACES
                       | LBRACES ExplicitConstructorInvocation RBRACES
                       | LBRACES MultBlockStatement RBRACES
                       | LBRACES RBRACES '''
    global globalST, currentST, STR


def p_ExplicitConstructorInvocation(p):
    '''ExplicitConstructorInvocation : TypeArguments THIS LPAREN ArgumentList RPAREN SEMICOLON
                                     | TypeArguments THIS LPAREN RPAREN SEMICOLON
                                     | THIS LPAREN ArgumentList RPAREN SEMICOLON
                                     | THIS LPAREN RPAREN SEMICOLON
                                     | TypeArguments SUPER LPAREN ArgumentList RPAREN SEMICOLON
                                     | TypeArguments SUPER LPAREN RPAREN SEMICOLON
                                     | SUPER LPAREN ArgumentList RPAREN SEMICOLON
                                     | SUPER LPAREN RPAREN SEMICOLON
                                     | TypeName DOT TypeArguments SUPER LPAREN ArgumentList RPAREN SEMICOLON
                                     | TypeName DOT TypeArguments SUPER LPAREN RPAREN SEMICOLON
                                     | TypeName DOT SUPER LPAREN ArgumentList RPAREN SEMICOLON
                                     | TypeName DOT SUPER LPAREN RPAREN SEMICOLON
                                     | Identifier DOT TypeArguments SUPER LPAREN ArgumentList RPAREN SEMICOLON
                                     | Identifier DOT TypeArguments SUPER LPAREN RPAREN SEMICOLON
                                     | Identifier DOT SUPER LPAREN ArgumentList RPAREN SEMICOLON
                                     | Identifier DOT SUPER LPAREN RPAREN SEMICOLON
                                     | Primary DOT TypeArguments SUPER LPAREN ArgumentList RPAREN SEMICOLON
                                     | Primary DOT TypeArguments SUPER LPAREN RPAREN SEMICOLON
                                     | Primary DOT SUPER LPAREN ArgumentList RPAREN SEMICOLON
                                     | Primary DOT SUPER LPAREN RPAREN SEMICOLON '''
    global globalST, currentST, STR


def p_EnumDeclaration(p):
    '''EnumDeclaration : ModifierList ENUM Identifier Superinterfaces EnumBody
                       | ModifierList ENUM Identifier EnumBody
                       | ENUM Identifier Superinterfaces EnumBody
                       | ENUM Identifier EnumBody'''
    global globalST, currentST, STR


def p_EnumBody(p):
    '''EnumBody : LBRACES EnumConstantList COMMA EnumBodyDeclarations RBRACES
                | LBRACES EnumConstantList COMMA RBRACES
                | LBRACES EnumConstantList EnumBodyDeclarations RBRACES
                | LBRACES Identifier COMMA EnumBodyDeclarations RBRACES
                | LBRACES Identifier COMMA RBRACES
                | LBRACES Identifier EnumBodyDeclarations RBRACES
                | LBRACES COMMA EnumBodyDeclarations RBRACES
                | LBRACES EnumBodyDeclarations RBRACES
                | LBRACES EnumConstantList RBRACES
                | LBRACES Identifier RBRACES
                | LBRACES COMMA RBRACES
                | LBRACES RBRACES '''
    global globalST, currentST, STR


def p_EnumConstantList(p):
    '''EnumConstantList : EnumConstant COMMA EnumConstantList
                        | Identifier COMMA EnumConstantList
                        | Identifier COMMA Identifier
                        | EnumConstant'''
    global globalST, currentST, STR


def p_EnumConstant(p):
    '''EnumConstant : MultEnumConstantModifier Identifier LPAREN ArgumentList RPAREN ClassBody
                    | MultEnumConstantModifier Identifier LPAREN ArgumentList RPAREN
                    | MultEnumConstantModifier Identifier LPAREN RPAREN ClassBody
                    | Identifier LPAREN ArgumentList RPAREN ClassBody
                    | Identifier LPAREN RPAREN ClassBody
                    | Identifier LPAREN ArgumentList RPAREN
                    | MultEnumConstantModifier Identifier LPAREN RPAREN
                    | Identifier LPAREN RPAREN
                    | MultEnumConstantModifier Identifier ClassBody
                    | MultEnumConstantModifier Identifier
                    | Identifier ClassBody
                    '''
    global globalST, currentST, STR


def p_MultEnumConstantModifier(p):
    '''MultEnumConstantModifier : EnumConstantModifier MultEnumConstantModifier
                                | EnumConstantModifier'''
    global globalST, currentST, STR


def p_EnumConstantModifier(p):
    '''EnumConstantModifier : Annotation'''
    global globalST, currentST, STR


def p_EnumBodyDeclarations(p):
    '''EnumBodyDeclarations : SEMICOLON MultClassBodyDeclaration
                            | SEMICOLON '''
    global globalST, currentST, STR


def p_InterfaceDeclaration(p):
    '''InterfaceDeclaration : NormalInterfaceDeclaration
                            | AnnotationTypeDeclaration'''
    global globalST, currentST, STR


def p_NormalInterfaceDeclaration(p):
    '''NormalInterfaceDeclaration : ModifierList INTERFACE Identifier TypeParameters ExtendsInterfaces InterfaceBody
                                  | ModifierList INTERFACE Identifier ExtendsInterfaces InterfaceBody
                                  | ModifierList INTERFACE Identifier TypeParameters InterfaceBody
                                  | ModifierList INTERFACE Identifier InterfaceBody
                                  | INTERFACE Identifier TypeParameters ExtendsInterfaces InterfaceBody
                                  | INTERFACE Identifier ExtendsInterfaces InterfaceBody
                                  | INTERFACE Identifier TypeParameters InterfaceBody
                                  | INTERFACE Identifier InterfaceBody'''
    global globalST, currentST, STR


def p_ExtendsInterfaces(p):
    '''ExtendsInterfaces : EXTENDS InterfaceTypeList '''
    global globalST, currentST, STR


def p_InterfaceBody(p):
    '''InterfaceBody : LBRACES MultInterfaceMemberDeclaration RBRACES
                     | LBRACES RBRACES'''
    global globalST, currentST, STR


def p_InterfaceMemberDeclaration(p):
    '''InterfaceMemberDeclaration : ConstantDeclaration
                                  | InterfaceMethodDeclaration
                                  | ClassDeclaration
                                  | InterfaceDeclaration
                                  | SEMICOLON'''
    global globalST, currentST, STR


def p_MultInterfaceMemberDeclaration(p):
    '''MultInterfaceMemberDeclaration : InterfaceMemberDeclaration MultInterfaceMemberDeclaration
                                      | InterfaceMemberDeclaration'''
    global globalST, currentST, STR

#Added Type <--> Identifier to remove ClassType->Identifier
def p_ConstantDeclaration(p):
    '''ConstantDeclaration : ModifierList Type VariableDeclaratorList SEMICOLON
                           | ModifierList Type Identifier SEMICOLON
                           | Type VariableDeclaratorList SEMICOLON
                           | Type Identifier SEMICOLON
                           | ModifierList Identifier VariableDeclaratorList SEMICOLON
                           | ModifierList Identifier Identifier SEMICOLON
                           | Identifier VariableDeclaratorList SEMICOLON
                           | Identifier Identifier SEMICOLON'''
    global globalST, currentST, STR


def p_InterfaceMethodDeclaration(p):
    '''InterfaceMethodDeclaration : ModifierList MethodHeader MethodBody
                                  | MethodHeader MethodBody'''
    global globalST, currentST, STR


def p_AnnotationTypeDeclaration(p):
    '''AnnotationTypeDeclaration : ModifierList AT INTERFACE Identifier AnnotationTypeBody
                                 | AT INTERFACE Identifier AnnotationTypeBody'''
    global globalST, currentST, STR


def p_AnnotationTypeBody(p):
    '''AnnotationTypeBody : LBRACES MultAnnotationTypeMemberDeclaration RBRACES
                          | LBRACES RBRACES'''
    global globalST, currentST, STR


def p_MultAnnotationTypeMemberDeclaration(p):
    '''MultAnnotationTypeMemberDeclaration : AnnotationTypeMemberDeclaration MultAnnotationTypeMemberDeclaration
                                           | AnnotationTypeMemberDeclaration'''
    global globalST, currentST, STR


def p_AnnotationTypeMemberDeclaration(p):
    '''AnnotationTypeMemberDeclaration : AnnotationTypeElementDeclaration
                                       | ConstantDeclaration
                                       | ClassDeclaration
                                       | InterfaceDeclaration
                                       | SEMICOLON '''
    global globalST, currentST, STR


#Added Type <--> Identifier to remove ClassType->Identifier
def p_AnnotationTypeElementDeclaration(p):
    '''AnnotationTypeElementDeclaration : MultAnnotationTypeElementModifier Type Identifier LPAREN RPAREN Dims DefaultValue SEMICOLON
                                        | Type Identifier LPAREN RPAREN Dims DefaultValue SEMICOLON
                                        | MultAnnotationTypeElementModifier Type Identifier LPAREN RPAREN DefaultValue SEMICOLON
                                        | MultAnnotationTypeElementModifier Type Identifier LPAREN RPAREN Dims SEMICOLON
                                        | MultAnnotationTypeElementModifier Type Identifier LPAREN RPAREN SEMICOLON
                                        | Type Identifier LPAREN RPAREN DefaultValue SEMICOLON
                                        | Type Identifier LPAREN RPAREN Dims SEMICOLON
                                        | Type Identifier LPAREN RPAREN SEMICOLON 
                                        | MultAnnotationTypeElementModifier Identifier Identifier LPAREN RPAREN Dims DefaultValue SEMICOLON
                                        | Identifier Identifier LPAREN RPAREN Dims DefaultValue SEMICOLON
                                        | MultAnnotationTypeElementModifier Identifier Identifier LPAREN RPAREN DefaultValue SEMICOLON
                                        | MultAnnotationTypeElementModifier Identifier Identifier LPAREN RPAREN Dims SEMICOLON
                                        | MultAnnotationTypeElementModifier Identifier Identifier LPAREN RPAREN SEMICOLON
                                        | Identifier Identifier LPAREN RPAREN DefaultValue SEMICOLON
                                        | Identifier Identifier LPAREN RPAREN Dims SEMICOLON
                                        | Identifier Identifier LPAREN RPAREN SEMICOLON '''
    global globalST, currentST, STR


def p_MultAnnotationTypeElementModifier(p):
    '''MultAnnotationTypeElementModifier : ModifierList '''
    global globalST, currentST, STR


def p_DefaultValue(p):
    '''DefaultValue :  DEFAULT ElementValue'''
    global globalST, currentST, STR


def p_Annotation(p):
    '''Annotation : NormalAnnotation
                  | MarkerAnnotation
                  | SingleElementAnnotation'''
    global globalST, currentST, STR

    p[0] = p[1]
    #  p[0] = {
        #  STR.ANNOTATIONS = p[1]
    #  }
    logger(p[0])


def p_NormalAnnotation(p):
    '''NormalAnnotation : AT TypeName LPAREN ElementValuePairList RPAREN 
                        | AT Identifier LPAREN ElementValuePairList RPAREN 
                        | AT TypeName LPAREN RPAREN 
                        | AT Identifier LPAREN RPAREN '''
    global globalST, currentST, STR
    
    p[0] = {
        STR.ANNOTATION_TYPE: STR.NORMAL_ANNOT
    }

    for i in range(2, len(p)):
        if (p[i] is not None):
            if STR.TYPE in p[i]:
                p[0][STR.TYPE] = p[i].get(STR.TYPE, [])

            if STR.IDENTIFIER_TYPE in p[i]:
                if (p[0].get(STR.TYPE, None) is None):
                    p[0][STR.TYPE] = p[i].get(STR.IDENTIFIER_TYPE, [])

            if STR.ELEM_VALUE_PAIR in p[i]:
                p[0][STR.ELEM_VALUE_PAIR] = p[i].get(STR.ELEM_VALUE_PAIR, [])
    logger(p[0])

def p_ElementValuePairList(p):
    '''ElementValuePairList : ElementValuePair COMMA ElementValuePairList 
                            | ElementValuePair
                            '''
    global globalST, currentST, STR
    p[0] = {
        STR.ELEM_VALUE_PAIR: p[1].get(STR.ELEM_VALUE_PAIR, []) if len(p) == 2 else p[1].get(STR.ELEM_VALUE_PAIR, []) + p[3].get(STR.ELEM_VALUE_PAIR, [])
    }

def p_ElementValuePair(p):
    '''ElementValuePair : Identifier EQUAL ElementValue'''
    global globalST, currentST, STR

    p[0] = {}
    #  p[0][STR.ANNOT_ID] = p[1]
    p[0][STR.ELEM_VALUE_PAIR] = [(p[1], p[3])]
    logger(p[0])

def p_ElementValue(p):
    '''ElementValue : ConditionalExpression
                    | ElementValueArrayInitializer
                    | Annotation '''
    global globalST, currentST, STR

    p[0] = {}
    if (p[1] is not None):
        if STR.TYPE in p[1]:
            p[0][STR.TYPE] = p[1].get(STR.TYPE)

        if STR.IDENTIFIER_TYPE in p[1]:
            if (p[0].get(STR.TYPE, None) is None):
                p[0][STR.TYPE] = p[1].get(STR.IDENTIFIER_TYPE)

        if STR.VALUE in p[1]:
            p[0][STR.ELEM_VALUE] = p[1].get(STR.VALUE)

    logger(p[0])

def p_ElementValueArrayInitializer(p):
    '''ElementValueArrayInitializer : LBRACES ElementValueList COMMA RBRACES
                                    | LBRACES ElementValueList RBRACES
                                    | LBRACES COMMA RBRACES
                                    | LBRACES RBRACES '''
    global globalST, currentST, STR

    p[0] = {
        STR.ELEM_VALUE : []
    }
    if (len(p) >= 4 and p[2] != ','):
        p[0][STR.ELEM_VALUE] = p[2]

def p_ElementValueList(p):
    '''ElementValueList : ElementValue COMMA ElementValueList
                        | ElementValue'''
    global globalST, currentST, STR

    p[0] = {
        STR.ELEM_VALUE: p[1].get(STR.ELEM_VALUE, []) if len(p) == 2 else p[1].get(STR.ELEM_VALUE, []) + p[3].get(STR.ELEM_VALUE, [])
    }

def p_MarkerAnnotation(p):
    '''MarkerAnnotation : AT TypeName 
                        | AT Identifier '''
    global globalST, currentST, STR

    p[0] = p[2]
    p[0][STR.ANNOTATION_TYPE] = STR.MARKER_ANNOT

def p_SingleElementAnnotation(p):
    '''SingleElementAnnotation : AT TypeName LPAREN ElementValue RPAREN
                               | AT Identifier LPAREN ElementValue RPAREN'''
    global globalST, currentST, STR

    p[0] = {
        STR.ANNOTATION_TYPE: STR.SINGLE_ELEM_ANNOT
    }

    if (p[1] is not None):
        if STR.TYPE in p[1]:
            p[0][STR.TYPE] = p[1].get(STR.TYPE)

        if STR.IDENTIFIER_TYPE in p[1]:
            if (p[0].get(STR.TYPE, None) is None):
                p[0][STR.TYPE] = p[1].get(STR.IDENTIFIER_TYPE)

    p[0][STR.ELEM_VALUE] = p[4]
    logger(p[0])

def p_ArrayInitializer(p):
    '''ArrayInitializer : LBRACES VariableInitializerList COMMA RBRACES
                        | LBRACES VariableInitializerList RBRACES
                        | LBRACES COMMA RBRACES
                        | LBRACES RBRACES
                        '''
    global globalST, currentST, STR


def p_VariableInitializerList(p):
    '''VariableInitializerList : VariableInitializer COMMA VariableInitializerList
                               | VariableInitializer'''
    global globalST, currentST, STR


def p_Block(p):
    '''Block : LBRACES MultBlockStatement RBRACES
             | LBRACES RBRACES '''
    global globalST, currentST, STR
   
    blocks = p[2].get(STR.BLOCKS, [])
    childBlocks = list(SymbolTable()) if len(p) == 3 else p[2].get(STR.BLOCKS)

    newParentBlock = SymbolTable()
    if (p[2].get(STR.BLOCKS) is not None):
        for childBlock in p[2].get(STR.BLOCKS):
            newParentBlock.addChildren(childBlock)
            childBlock.setParent(newParentBlock)

def p_MultBlockStatement(p):
    '''MultBlockStatement : BlockStatement MultBlockStatement
                          | BlockStatement'''
    global globalST, currentST, STR
    
    blocks = p[1].get(STR.BLOCKS, [])
    p[0] = {
        STR.BLOCKS: blocks if len(p) == 2 else blocks + p[2].get(STR.BLOCKS, [])
    }

def p_BlockStatement(p):
    '''BlockStatement : LocalVariableDeclaration
                      | ClassDeclaration
                      | Statement'''
    global globalST, currentST, STR

    p[0] = {
        STR.BLOCKS: [currentST]
    }
    
    currentST.printST()
    # Create a new SymbolTable HERE!
    currentST = SymbolTable()

#Added Type <--> Identifier to remove ClassType->Identifier
def p_LocalVariableDeclaration(p):
    '''LocalVariableDeclaration : ModifierList Type VariableDeclaratorList SEMICOLON
                                | ModifierList Type Identifier SEMICOLON
                                | ModifierList Identifier VariableDeclaratorList SEMICOLON
                                | ModifierList Identifier Identifier SEMICOLON
                                | Type VariableDeclaratorList SEMICOLON
                                | Type Identifier SEMICOLON 
                                | Identifier VariableDeclaratorList SEMICOLON
                                | Identifier Identifier SEMICOLON''' 
    global globalST, currentST, STR

    modifiers = list() if len(p) == 4 else p[1][STR.MODIFIERS]
    annotations = list() if len(p) == 4 else p[1][STR.ANNOTATIONS]

    k = p[1:] if len(p) == 4 else p[2:]

    logger(p[0])
    p[0] = {
        "ID": list() 
    }

    if (k[1] is not None and k[1]["ID"] is not None):
        for ID in k[1]["ID"]:
            ID_obj = {
                    "ID": ID,
                    STR.IDENTIFIER_TYPE: k[0].get(STR.IDENTIFIER_TYPE),
                    STR.ANNOTATIONS: annotations,
                    STR.MODIFIERS: modifiers 
            }
            p[0]["ID"].append(ID_obj)

            if (not currentST.insertSymbol(ID, ID_obj)):
                raise DeclarationError("Variable %s has already been declared in current scope. " % (ID))
    logger(p[0])

    

def p_Statement(p):
    '''Statement : StatementWithoutTrailingSubstatement
                 | LabeledStatement
                 | IfThenStatement
                 | IfThenElseStatement
                 | WhileStatement
                 | ForStatement'''
    global globalST, currentST, STR


def p_StatementNoShortIf(p):
    '''StatementNoShortIf : StatementWithoutTrailingSubstatement
                          | LabeledStatementNoShortIf
                          | IfThenElseStatementNoShortIf
                          | WhileStatementNoShortIf
                          | ForStatementNoShortIf'''
    global globalST, currentST, STR


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
    global globalST, currentST, STR


def p_EmptyStatement(p):
    '''EmptyStatement : SEMICOLON'''
    global globalST, currentST, STR


def p_LabeledStatement(p):
    '''LabeledStatement : Identifier COLON Statement '''
    global globalST, currentST, STR


def p_LabeledStatementNoShortIf(p):
    '''LabeledStatementNoShortIf : Identifier COLON StatementNoShortIf '''
    global globalST, currentST, STR


def p_ExpressionStatement(p):
    '''ExpressionStatement : StatementExpression SEMICOLON '''
    global globalST, currentST, STR


def p_StatementExpression(p):
    '''StatementExpression : Assignment
                           | PLUSPLUS UnaryExpression
                           | MINUSMINUS UnaryExpression
                           | PostIncrementExpression
                           | PostDecrementExpression
                           | MethodInvocation
                           | ClassInstanceCreationExpression'''
    global globalST, currentST, STR


def p_IfThenStatement(p):
    '''IfThenStatement :  IF LPAREN Expression RPAREN Statement'''
    global globalST, currentST, STR


def p_IfThenElseStatement(p):
    '''IfThenElseStatement : IF LPAREN Expression RPAREN StatementNoShortIf ELSE Statement '''
    global globalST, currentST, STR


def p_IfThenElseStatementNoShortIf(p):
    '''IfThenElseStatementNoShortIf : IF LPAREN Expression RPAREN StatementNoShortIf ELSE StatementNoShortIf '''
    global globalST, currentST, STR


def p_AssertStatement(p):
    '''AssertStatement : ASSERT Expression SEMICOLON
                       | ASSERT Expression COLON Expression SEMICOLON'''
    global globalST, currentST, STR


def p_SwitchStatement(p):
    '''SwitchStatement : SWITCH LPAREN Expression RPAREN SwitchBlock '''
    global globalST, currentST, STR


def p_SwitchBlock(p):
    '''SwitchBlock : LBRACES MultSwitchBlockStatementGroup MultSwitchLabel RBRACES
                   | LBRACES MultSwitchBlockStatementGroup RBRACES
                   | LBRACES MultSwitchLabel RBRACES
                   | LBRACES RBRACES '''
    global globalST, currentST, STR


def p_MultSwitchBlockStatementGroup(p):
    '''MultSwitchBlockStatementGroup : SwitchBlockStatementGroup MultSwitchBlockStatementGroup
                      | SwitchBlockStatementGroup'''
    global globalST, currentST, STR


def p_SwitchBlockStatementGroup(p):
    '''SwitchBlockStatementGroup :  MultSwitchLabel MultBlockStatement'''
    global globalST, currentST, STR


def p_MultSwitchLabel(p):
    '''MultSwitchLabel : SwitchLabel MultSwitchLabel
                       | SwitchLabel'''
    global globalST, currentST, STR


def p_SwitchLabel(p):
    '''SwitchLabel : CASE Expression COLON
                   | CASE Identifier COLON
                   | DEFAULT COLON'''
    global globalST, currentST, STR


def p_WhileStatement(p):
    '''WhileStatement : WHILE LPAREN Expression RPAREN Statement '''
    global globalST, currentST, STR


def p_WhileStatementNoShortIf(p):
    '''WhileStatementNoShortIf :  WHILE LPAREN Expression RPAREN StatementNoShortIf'''
    global globalST, currentST, STR


def p_DoStatement(p):
    '''DoStatement :  DO Statement WHILE LPAREN Expression RPAREN SEMICOLON'''
    global globalST, currentST, STR


def p_ForStatement(p):
    '''ForStatement : BasicForStatement
                    | EnhancedForStatement'''
    global globalST, currentST, STR


def p_ForStatementNoShortIf(p):
    '''ForStatementNoShortIf : BasicForStatementNoShortIf
                             | EnhancedForStatementNoShortIf '''
    global globalST, currentST, STR


def p_BasicForStatement(p):
    '''BasicForStatement : FOR LPAREN ForInit SEMICOLON Expression SEMICOLON ForUpdate RPAREN Statement
                         | FOR LPAREN ForInit SEMICOLON Expression SEMICOLON RPAREN Statement
                         | FOR LPAREN ForInit SEMICOLON SEMICOLON ForUpdate RPAREN Statement
                         | FOR LPAREN SEMICOLON Expression SEMICOLON ForUpdate RPAREN Statement
                         | FOR LPAREN SEMICOLON SEMICOLON ForUpdate RPAREN Statement
                         | FOR LPAREN SEMICOLON Expression SEMICOLON RPAREN Statement
                         | FOR LPAREN ForInit SEMICOLON SEMICOLON RPAREN Statement
                         | FOR LPAREN SEMICOLON SEMICOLON RPAREN Statement
                         '''
    global globalST, currentST, STR


def p_BasicForStatementNoShortIf(p):
    '''BasicForStatementNoShortIf : FOR LPAREN ForInit SEMICOLON Expression SEMICOLON ForUpdate RPAREN StatementNoShortIf
                                  | FOR LPAREN ForInit SEMICOLON Expression SEMICOLON RPAREN StatementNoShortIf
                                  | FOR LPAREN ForInit SEMICOLON SEMICOLON ForUpdate RPAREN StatementNoShortIf
                                  | FOR LPAREN SEMICOLON Expression SEMICOLON ForUpdate RPAREN StatementNoShortIf
                                  | FOR LPAREN SEMICOLON SEMICOLON ForUpdate RPAREN StatementNoShortIf
                                  | FOR LPAREN SEMICOLON Expression SEMICOLON RPAREN StatementNoShortIf
                                  | FOR LPAREN ForInit SEMICOLON SEMICOLON RPAREN StatementNoShortIf
                                  | FOR LPAREN SEMICOLON SEMICOLON RPAREN StatementNoShortIf'''
    global globalST, currentST, STR


def p_ForInit(p):
    '''ForInit : StatementExpressionList
               | LocalVariableDeclaration'''
    global globalST, currentST, STR


def p_ForUpdate(p):
    '''ForUpdate : StatementExpressionList'''
    global globalST, currentST, STR


def p_StatementExpressionList(p):
    '''StatementExpressionList : StatementExpression COMMA StatementExpressionList
                               | StatementExpression'''
    global globalST, currentST, STR


#Added Type <--> Identifier to remove ClassType->Identifier
def p_EnhancedForStatement(p):
    '''EnhancedForStatement : FOR LPAREN ModifierList Type VariableDeclaratorId COLON Expression RPAREN Statement
                            | FOR LPAREN ModifierList Type Identifier COLON Expression RPAREN Statement
                            | FOR LPAREN Type VariableDeclaratorId COLON Expression RPAREN Statement
                            | FOR LPAREN Type Identifier COLON Expression RPAREN Statement
                            | FOR LPAREN ModifierList Identifier VariableDeclaratorId COLON Expression RPAREN Statement
                            | FOR LPAREN ModifierList Identifier Identifier COLON Expression RPAREN Statement
                            | FOR LPAREN Identifier VariableDeclaratorId COLON Expression RPAREN Statement
                            | FOR LPAREN Identifier Identifier COLON Expression RPAREN Statement'''
    global globalST, currentST, STR


#Added Type <--> Identifier to remove ClassType->Identifier
def p_EnhancedForStatementNoShortIf(p):
    '''EnhancedForStatementNoShortIf : FOR LPAREN ModifierList Type VariableDeclaratorId COLON Expression RPAREN StatementNoShortIf
                                     | FOR LPAREN ModifierList Type Identifier COLON Expression RPAREN StatementNoShortIf
                                     | FOR LPAREN Type VariableDeclaratorId COLON Expression RPAREN StatementNoShortIf
                                     | FOR LPAREN Type Identifier COLON Expression RPAREN StatementNoShortIf
                                     | FOR LPAREN ModifierList Identifier VariableDeclaratorId COLON Expression RPAREN StatementNoShortIf
                                     | FOR LPAREN ModifierList Identifier Identifier COLON Expression RPAREN StatementNoShortIf
                                     | FOR LPAREN Identifier VariableDeclaratorId COLON Expression RPAREN StatementNoShortIf
                                     | FOR LPAREN Identifier Identifier COLON Expression RPAREN StatementNoShortIf'''
    global globalST, currentST, STR


def p_BreakStatement(p):
    '''BreakStatement : BREAK Identifier SEMICOLON
                      | BREAK SEMICOLON'''
    global globalST, currentST, STR


def p_ContinueStatement(p):
    '''ContinueStatement : CONTINUE Identifier SEMICOLON
                         | CONTINUE SEMICOLON '''
    global globalST, currentST, STR


def p_ReturnStatement(p):
    '''ReturnStatement : RETURN Expression SEMICOLON
                       | RETURN SEMICOLON'''
    global globalST, currentST, STR

    p[2][STR.TYPE] == symboltable.type

def p_ThrowStatement(p):
    '''ThrowStatement : THROW Expression SEMICOLON '''
    global globalST, currentST, STR


def p_SynchronizedStatement(p):
    '''SynchronizedStatement : SYNCHRONIZED LPAREN Expression RPAREN Block '''
    global globalST, currentST, STR


def p_TryStatement(p):
    '''TryStatement : TRY Block Catches Finally
                    | TRY Block Catches
                    | TRY Block Finally
                    | TryWithResourcesStatement '''
    global globalST, currentST, STR


def p_Catches(p):
    '''Catches : MultCatchClause'''
    global globalST, currentST, STR


def p_MultCatchClause(p):
    '''MultCatchClause : CatchClause MultCatchClause
                       | CatchClause'''
    global globalST, currentST, STR


def p_CatchClause(p):
    '''CatchClause : CATCH LPAREN CatchFormalParameter RPAREN Block '''
    global globalST, currentST, STR


def p_CatchFormalParameter(p):
    '''CatchFormalParameter : ModifierList CatchType VariableDeclaratorId
                            | ModifierList CatchType Identifier
                            | CatchType VariableDeclaratorId
                            | CatchType Identifier
                            | Identifier Identifier'''
    global globalST, currentST, STR


def p_CatchType(p):
    '''CatchType : CatchType BOOLEANOR ClassType
                 | CatchType BOOLEANOR Identifier
                 | Identifier BOOLEANOR Identifier
                 | ClassType '''
    global globalST, currentST, STR


def p_Finally(p):
    '''Finally : FINALLY Block '''
    global globalST, currentST, STR


def p_TryWithResourcesStatement(p):
    '''TryWithResourcesStatement : TRY ResourceSpecification Block Catches Finally
                                 | TRY ResourceSpecification Block Catches
                                 | TRY ResourceSpecification Block Finally
                                 | TRY ResourceSpecification Block '''
    global globalST, currentST, STR


def p_ResourceSpecification(p):
    '''ResourceSpecification : LPAREN ResourceList SEMICOLON RPAREN
                             | LPAREN ResourceList RPAREN '''
    global globalST, currentST, STR


def p_ResourceList(p):
    '''ResourceList : Resource SEMICOLON ResourceList
                    | Resource'''
    global globalST, currentST, STR


#Added Type <--> Identifier to remove ClassType->Identifier
def p_Resource(p):
    '''Resource : ModifierList Type VariableDeclaratorId EQUAL Expression
                | ModifierList Type Identifier EQUAL Expression
                | Type VariableDeclaratorId EQUAL Expression
                | Type Identifier EQUAL Expression
                | ModifierList Identifier VariableDeclaratorId EQUAL Expression
                | ModifierList Identifier Identifier EQUAL Expression
                | Identifier VariableDeclaratorId EQUAL Expression
                | Identifier Identifier EQUAL Expression'''
    global globalST, currentST, STR


def p_Primary(p):
    '''Primary : PrimaryNoNewArray
               | ArrayCreationExpression '''
    global globalST, currentST, STR

    p[0] = {
        STR.TYPE: p[1][STR.TYPE],
        STR.VALUE: p[1].get(STR.VALUE, None)
    }

def p_PrimaryNoNewArray(p):
    '''PrimaryNoNewArray : Literal
                         | ClassLiteral
                         | THIS
                         | TypeName DOT THIS
                         | Identifier DOT THIS
                         | LPAREN Expression RPAREN
                         | ClassInstanceCreationExpression
                         | FieldAccess
                         | ArrayAccess
                         | MethodInvocation
                         | MethodReference'''
    global globalST, currentST, STR

    #TODO: Change IDENTIFIER_TYPE to others as well
    p[0] = {
        STR.VALUE: p[1].get(STR.VALUE, None)
    }
    if p[1] is not None:
        if STR.IDENTIFIER_TYPE in p[1]:
            p[0][STR.TYPE] = p[1][STR.IDENTIFIER_TYPE]

        if STR.TYPE in p[1]:
            p[0][STR.TYPE] = p[1][STR.TYPE]

        if STR.VALUE in p[1]:
            p[0][STR.VALUE] = p[1][STR.VALUE]

def p_ClassLiteral(p):
    '''ClassLiteral : TypeName Brackets DOT CLASS
                    | Identifier Brackets DOT CLASS
                    | NumericType Brackets DOT CLASS
                    | BooleanType Brackets DOT CLASS
                    | TypeName DOT CLASS
                    | Identifier DOT CLASS
                    | Identifier DOT Identifier
                    | NumericType DOT CLASS
                    | BooleanType DOT CLASS
                    | VOID DOT CLASS'''
    global globalST, currentST, STR


def p_ClassInstanceCreationExpression(p):
    '''ClassInstanceCreationExpression : UnqualifiedClassInstanceCreationExpression
                                       | Identifier DOT UnqualifiedClassInstanceCreationExpression
                                       | TypeName DOT UnqualifiedClassInstanceCreationExpression
                                       | Primary DOT UnqualifiedClassInstanceCreationExpression'''
    global globalST, currentST, STR


def p_UnqualifiedClassInstanceCreationExpression(p):
    '''UnqualifiedClassInstanceCreationExpression : NEW TypeArguments ClassOrInterfaceTypeToInstantiate LPAREN ArgumentList RPAREN ClassBody
                                                  | NEW ClassOrInterfaceTypeToInstantiate LPAREN ArgumentList RPAREN ClassBody
                                                  | NEW TypeArguments ClassOrInterfaceTypeToInstantiate LPAREN RPAREN ClassBody
                                                  | NEW TypeArguments ClassOrInterfaceTypeToInstantiate LPAREN ArgumentList RPAREN
                                                  | NEW TypeArguments ClassOrInterfaceTypeToInstantiate LPAREN RPAREN
                                                  | NEW ClassOrInterfaceTypeToInstantiate LPAREN ArgumentList RPAREN
                                                  | NEW ClassOrInterfaceTypeToInstantiate LPAREN RPAREN ClassBody
                                                  | NEW ClassOrInterfaceTypeToInstantiate LPAREN RPAREN 
                                                  | NEW TypeArguments Identifier LPAREN ArgumentList RPAREN ClassBody
                                                  | NEW Identifier LPAREN ArgumentList RPAREN ClassBody
                                                  | NEW TypeArguments Identifier LPAREN RPAREN ClassBody
                                                  | NEW TypeArguments Identifier LPAREN ArgumentList RPAREN
                                                  | NEW TypeArguments Identifier LPAREN RPAREN
                                                  | NEW Identifier LPAREN ArgumentList RPAREN
                                                  | NEW Identifier LPAREN RPAREN ClassBody
                                                  | NEW Identifier LPAREN RPAREN '''
    global globalST, currentST, STR


def p_ClassOrInterfaceTypeToInstantiate(p):
    '''ClassOrInterfaceTypeToInstantiate : MultAnnotation Identifier ClassOrInterfaceTypeToInstantiate1 TypeArgumentsOrDiamond
                                         | MultAnnotation Identifier TypeArgumentsOrDiamond
                                         | MultAnnotation Identifier ClassOrInterfaceTypeToInstantiate1
                                         | MultAnnotation Identifier
                                         | Identifier ClassOrInterfaceTypeToInstantiate1 TypeArgumentsOrDiamond
                                         | Identifier TypeArgumentsOrDiamond
                                         | Identifier ClassOrInterfaceTypeToInstantiate1
                                         '''
    global globalST, currentST, STR


def p_ClassOrInterfaceTypeToInstantiate1(p):
    '''ClassOrInterfaceTypeToInstantiate1 : DOT MultAnnotation Identifier ClassOrInterfaceTypeToInstantiate1
                                          | DOT Identifier ClassOrInterfaceTypeToInstantiate1
                                          | DOT MultAnnotation Identifier 
                                          | DOT Identifier '''
    global globalST, currentST, STR


def p_TypeArgumentsOrDiamond(p):
    '''TypeArgumentsOrDiamond : TypeArguments '''
    global globalST, currentST, STR


def p_FieldAccess(p):
    '''FieldAccess : Primary DOT Identifier
                   | SUPER DOT Identifier
                   | Identifier DOT SUPER DOT Identifier
                   | TypeName DOT SUPER DOT Identifier'''
    global globalST, currentST, STR


def p_ArrayAccess(p):
    '''ArrayAccess : TypeName LBRACKETS Expression RBRACKETS
                   | Identifier LBRACKETS Expression RBRACKETS
                   | PrimaryNoNewArray LBRACKETS Expression RBRACKETS '''
    global globalST, currentST, STR


def p_MethodInvocation(p):
    '''MethodInvocation : Identifier LPAREN ArgumentList RPAREN
                        | Identifier LPAREN RPAREN
                        | TypeName DOT TypeArguments Identifier LPAREN ArgumentList RPAREN
                        | TypeName DOT Identifier LPAREN ArgumentList RPAREN
                        | TypeName DOT TypeArguments Identifier LPAREN RPAREN
                        | TypeName DOT Identifier LPAREN RPAREN
                        | Identifier DOT TypeArguments Identifier LPAREN ArgumentList RPAREN
                        | Identifier DOT Identifier LPAREN ArgumentList RPAREN
                        | Identifier DOT TypeArguments Identifier LPAREN RPAREN
                        | Identifier DOT Identifier LPAREN RPAREN
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
                        | TypeName DOT SUPER DOT Identifier LPAREN RPAREN 
                        | Identifier DOT SUPER DOT TypeArguments Identifier LPAREN ArgumentList RPAREN
                        | Identifier DOT SUPER DOT Identifier LPAREN ArgumentList RPAREN
                        | Identifier DOT SUPER DOT TypeArguments Identifier LPAREN RPAREN
                        | Identifier DOT SUPER DOT Identifier LPAREN RPAREN '''
    global globalST, currentST, STR


def p_ArgumentList(p):
    '''ArgumentList : Expression COMMA ArgumentList
                    | Expression'''
    global globalST, currentST, STR


def p_MethodReference(p):
    '''MethodReference : TypeName DOUBLECOLON TypeArguments Identifier
                       | TypeName DOUBLECOLON Identifier
                       | Identifier DOUBLECOLON TypeArguments Identifier
                       | Identifier DOUBLECOLON Identifier
                       | ReferenceType DOUBLECOLON TypeArguments Identifier
                       | ReferenceType DOUBLECOLON Identifier
                       | Primary DOUBLECOLON TypeArguments Identifier
                       | Primary DOUBLECOLON Identifier
                       | SUPER DOUBLECOLON TypeArguments Identifier
                       | SUPER DOUBLECOLON Identifier
                       | TypeName DOT SUPER DOUBLECOLON TypeArguments Identifier
                       | TypeName DOT SUPER DOUBLECOLON Identifier
                       | Identifier DOT SUPER DOUBLECOLON TypeArguments Identifier
                       | Identifier DOT SUPER DOUBLECOLON Identifier
                       | ClassType DOUBLECOLON TypeArguments NEW
                       | ClassType DOUBLECOLON NEW
                       | ArrayType DOUBLECOLON NEW '''
    global globalST, currentST, STR


def p_ArrayCreationExpression(p):
    '''ArrayCreationExpression : NEW PrimitiveType DimExprs Dims
                               | NEW ClassOrInterfaceType DimExprs Dims
                               | NEW Identifier DimExprs Dims
                               | NEW PrimitiveType DimExprs
                               | NEW ClassOrInterfaceType DimExprs
                               | NEW Identifier DimExprs
                               | NEW PrimitiveType Dims ArrayInitializer
                               | NEW ClassOrInterfaceType Dims ArrayInitializer
                               | NEW Identifier Dims ArrayInitializer'''
    global globalST, currentST, STR


def p_DimExprs(p):
    '''DimExprs : DimExpr DimExprs
                | DimExpr'''
    global globalST, currentST, STR


def p_DimExpr(p):
    '''DimExpr : MultAnnotation LBRACKETS Expression RBRACKETS
               | LBRACKETS Expression RBRACKETS'''
    global globalST, currentST, STR


def p_Expression(p):
    '''Expression : LambdaExpression
                  | AssignmentExpression '''
    global globalST, currentST, STR

    p[0] = {
        STR.TYPE: p[1][STR.TYPE],
    }

def p_LambdaExpression(p):
    '''LambdaExpression : LambdaParameters ARROW LambdaBody
                        | Identifier ARROW LambdaBody '''
    global globalST, currentST, STR


def p_LambdaParameters(p):
    '''LambdaParameters : LPAREN FormalParameterList RPAREN
                        | LPAREN RPAREN
                        | LPAREN CommaSeparatedIdentifiers RPAREN
                        | LPAREN Identifier RPAREN '''
    global globalST, currentST, STR


def p_LambdaBody(p):
    '''LambdaBody : Expression
                  | Block '''
    global globalST, currentST, STR


def p_AssignmentExpression(p):
    '''AssignmentExpression : ConditionalExpression
                            | Assignment '''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: p[1][STR.TYPE],
        STR.VALUE: p[1].get(STR.VALUE, []) if len(p) == 2 else []
    }

def p_Assignment(p):
    '''Assignment : LeftHandSide AssignmentOperator Expression
                  | TypeName AssignmentOperator Expression
                  | Identifier AssignmentOperator Expression'''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: assertExpressionType(None, p[1], p[3]),
    }

def p_LeftHandSide(p):
    '''LeftHandSide : FieldAccess
                    | ArrayAccess '''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: p[1][STR.TYPE] 
    }

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
    global globalST, currentST, STR

    logger(p[1:])

def p_ConditionalExpression(p):
    '''ConditionalExpression : ConditionalOrExpression QUESTIONMARK Expression COLON ConditionalExpression
                             | ConditionalOrExpression QUESTIONMARK Expression COLON LambdaExpression
                             | ConditionalOrExpression
                             '''
    global globalST, currentST, STR

    #TODO:
    p[0] = {
        STR.TYPE: assertExpressionType(None, p[1]) if len(p) == 2
                else assertExpressionType(NUMERIC_TYPES, p[1], p[3]),
        STR.VALUE: p[1].get(STR.VALUE) if len(p) == 2 else None
    }
    logger(p[0])

def p_ConditionalOrExpression(p):
    '''ConditionalOrExpression : ConditionalOrExpression OR ConditionalAndExpression
                               | ConditionalAndExpression
                               '''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: assertExpressionType(None, p[1]) if len(p) == 2
                else assertExpressionType(NUMERIC_TYPES, p[1], p[3]),
        STR.VALUE: p[1].get(STR.VALUE, []) if len(p) == 2 else None
    }

def p_ConditionalAndExpression(p):
    '''ConditionalAndExpression : ConditionalAndExpression AND InclusiveOrExpression
                                | InclusiveOrExpression
                                '''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: assertExpressionType(None, p[1]) if len(p) == 2
                else assertExpressionType(NUMERIC_TYPES, p[1], p[3]),
        STR.VALUE: p[1].get(STR.VALUE, []) if len(p) == 2 else None
    }

def p_InclusiveOrExpression(p):
    '''InclusiveOrExpression : InclusiveOrExpression BOOLEANOR ExclusiveOrExpression
                             | ExclusiveOrExpression
                             '''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: assertExpressionType(None, p[1]) if len(p) == 2
                else assertExpressionType(NUMERIC_TYPES, p[1], p[3]),
        STR.VALUE: p[1].get(STR.VALUE) if len(p) == 2 else None
    }

def p_ExclusiveOrExpression(p):
    '''ExclusiveOrExpression : ExclusiveOrExpression BOOLEANXOR AndExpression
                             | AndExpression
                            '''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: assertExpressionType(None, p[1]) if len(p) == 2
                else assertExpressionType(NUMERIC_TYPES, p[1], p[3]),
        STR.VALUE: p[1].get(STR.VALUE) if len(p) == 2 else None
    }
    p[0][STR.VALUE] = p[1].get(STR.VALUE, []) if len(p) == 2 else []

def p_AndExpression(p):
    '''AndExpression : AndExpression BOOLEANAND EqualityExpression
                     | EqualityExpression
                     '''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: assertExpressionType(None, p[1]) if len(p) == 2
                else assertExpressionType(NUMERIC_TYPES, p[1], p[3]),
        STR.VALUE: p[1].get(STR.VALUE) if len(p) == 2 else None
    }

def p_EqualityExpression(p):
    '''EqualityExpression : EqualityExpression EQUALS RelationalExpression
                          | EqualityExpression NOTEQUALS RelationalExpression 
                          | RelationalExpression
                          '''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: assertExpressionType(None, p[1]) if len(p) == 2
                else assertExpressionType(NUMERIC_TYPES, p[1], p[3]),
        STR.VALUE: p[1].get(STR.VALUE) if len(p) == 2 else None
    }
    p[0][STR.VALUE] = p[1].get(STR.VALUE, []) if len(p) == 2 else []

def p_RelationalExpression(p):
    '''RelationalExpression : RelationalExpression LESSTHAN ShiftExpression
                            | RelationalExpression GREATERTHAN ShiftExpression
                            | RelationalExpression LESSTHANEQUAL ShiftExpression
                            | RelationalExpression GREATERTHANEQUAL ShiftExpression
                            | RelationalExpression INSTANCEOF ReferenceType
                            | RelationalExpression INSTANCEOF Identifier
                            | ShiftExpression'''
    global globalST, currentST, STR

    logger(p[1:])

    p[0] = {
        STR.TYPE: ''
    }

    if (len(p) == 2):
        p[0][STR.TYPE] = assertExpressionType(None, p[1])
    else:
        if (p[2] != 'instanceof'):
            p[0][STR.TYPE] = assertExpressionType(NUMERIC_TYPES, p[1], p[3])
        else:
            #TODO: Handle instanceof case
            pass

    p[0][STR.VALUE] = p[1].get(STR.VALUE, []) if len(p) == 2 else []

def p_ShiftExpression(p):
    '''ShiftExpression : ShiftExpression LEFTSHIFT AdditiveExpression
                       | ShiftExpression RIGHTSHIFT AdditiveExpression
                       | ShiftExpression URIGHTSHIFT AdditiveExpression
                       | AdditiveExpression
                       '''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: assertExpressionType(None, p[1]) if len(p) == 2
                else assertExpressionType(INTEGRAL_TYPES, p[1], p[3]),
        STR.VALUE: p[1].get(STR.VALUE) if len(p) == 2 else None
    }


#Removed left recursion
def p_AdditiveExpression(p):
    '''AdditiveExpression : AdditiveExpression PLUS MultiplicativeExpression
                          | AdditiveExpression MINUS MultiplicativeExpression
                          | MultiplicativeExpression'''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: assertExpressionType(None, p[1]) if len(p) == 2
                else assertExpressionType(NUMERIC_TYPES + STRING_TYPES, p[1], p[3]),
        STR.VALUE: p[1].get(STR.VALUE, []) if len(p) == 2 else []
    }

def p_MultiplicativeExpression(p):
    '''MultiplicativeExpression : MultiplicativeExpression MULTIPLY UnaryExpression
                                | MultiplicativeExpression DIVIDE UnaryExpression
                                | MultiplicativeExpression MODULO UnaryExpression
                                | UnaryExpression '''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: assertExpressionType(None, p[1]) if len(p) == 2
                else assertExpressionType(NUMERIC_TYPES, p[1], p[3]),
        STR.VALUE: p[1].get(STR.VALUE, []) if len(p) == 2 else []
    }

def p_UnaryExpression(p):
    '''UnaryExpression : PLUSPLUS UnaryExpression
                       | MINUSMINUS UnaryExpression
                       | PLUS UnaryExpression
                       | MINUS UnaryExpression
                       | UnaryExpressionNotPlusMinus
                       | Primary
                       | TypeName
                       | Identifier '''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE : None
    }
    if (len(p) == 2):
        if p[1] is not None:
            if STR.IDENTIFIER_TYPE in p[1]:
                p[0][STR.TYPE] = p[1][STR.IDENTIFIER_TYPE]

            if STR.TYPE in p[1]:
                p[0][STR.TYPE] = p[1][STR.TYPE]

            if STR.VALUE in p[1]:
                p[0][STR.VALUE] = p[1][STR.VALUE]
    else:
        p[0][STR.TYPE] = assertExpressionType(NUMERIC_TYPES, p[2])

    logger(p[0])

def p_UnaryExpressionNotPlusMinus(p):
    '''UnaryExpressionNotPlusMinus : PostIncrementExpression
                                   | PostDecrementExpression
                                   | TILDA UnaryExpression
                                   | BOOLEANNOT UnaryExpression
                                   | CastExpression '''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: assertExpressionType(None, p[1]) if len(p) == 2 else assertExpressionType(NUMERIC_TYPES, p[2]),
        #  STR.VALUE: p[1].get(STR.VALUE, [])
    }


def p_PostIncrementExpression(p):
    '''PostIncrementExpression : PostIncrementExpression PLUSPLUS
                               | PostDecrementExpression PLUSPLUS
                               | Primary PLUSPLUS
                               | TypeName PLUSPLUS
                               | Identifier PLUSPLUS
                               '''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: assertExpressionType(None, p[1]) if len(p) == 2 else assertExpressionType(NUMERIC_TYPES, p[2]),
        STR.VALUE: p[1].get(STR.VALUE, [])
    }


def p_PostDecrementExpression(p):
    '''PostDecrementExpression : PostIncrementExpression MINUSMINUS
                               | PostDecrementExpression MINUSMINUS
                               | Primary MINUSMINUS
                               | TypeName MINUSMINUS
                               | Identifier MINUSMINUS
                               '''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: assertExpressionType(None, p[1]) if len(p) == 2 else assertExpressionType(NUMERIC_TYPES, p[2]),
        #  STR.VALUE: p[1].get(STR.VALUE, [])
    }


def p_CastExpression(p):
    '''CastExpression : LPAREN PrimitiveType RPAREN UnaryExpression
                      | LPAREN ReferenceType MultAdditionalBound RPAREN UnaryExpressionNotPlusMinus
                      | LPAREN ReferenceType RPAREN UnaryExpressionNotPlusMinus
                      | LPAREN ReferenceType MultAdditionalBound RPAREN Primary
                      | LPAREN ReferenceType RPAREN Primary
                      | LPAREN ReferenceType MultAdditionalBound RPAREN TypeName
                      | LPAREN ReferenceType RPAREN TypeName
                      | LPAREN ReferenceType MultAdditionalBound RPAREN Identifier
                      | LPAREN ReferenceType RPAREN Identifier
                      | LPAREN ReferenceType MultAdditionalBound RPAREN LambdaExpression
                      | LPAREN ReferenceType RPAREN LambdaExpression
                      | LPAREN Identifier MultAdditionalBound RPAREN UnaryExpressionNotPlusMinus
                      | LPAREN Identifier RPAREN UnaryExpressionNotPlusMinus
                      | LPAREN Identifier MultAdditionalBound RPAREN Primary
                      | LPAREN Identifier RPAREN Primary
                      | LPAREN Identifier MultAdditionalBound RPAREN TypeName
                      | LPAREN Identifier RPAREN TypeName
                      | LPAREN Identifier MultAdditionalBound RPAREN Identifier
                      | LPAREN Identifier RPAREN Identifier
                      | LPAREN Identifier MultAdditionalBound RPAREN LambdaExpression
                      | LPAREN Identifier RPAREN LambdaExpression
                      '''
    global globalST, currentST, STR

    logger(p[1:])
    p[0] = {
        STR.TYPE: p[2][STR.TYPE],
        #  STR.VALUE: p[1].get(STR.VALUE, [])
    }

################################################################################

#Default Error
def p_error(p):
    print("[Parser Error] at ", p, " (EXITING)")
    sys.exit(-1)
    return

def p_Brackets(p):
    ''' Brackets : LBRACKETS RBRACKETS Brackets
               | LBRACKETS RBRACKETS'''
    global globalST, currentST, STR

# Build the parser
if __name__ == '__main__':
    parser = yacc.yacc()

    file=open(sys.argv[1],'r')
    test=file.read()

    file_path = sys.argv[1]
    file1 = open(file_path)
    code = file1.read()


    #  parser.parse(code,lexer, True, True)
    parser.parse(code,lexer, False, True)

    print("Parsed successfully! Bye bye!")
