grammar Expression;

expr: expression EOF;

exprList: LPAREN expression (SEMI expression)+ RPAREN;

expression:	logicalOrExpression 
		( 
		  (ASSIGN logicalOrExpression) 
		| (DEFAULT logicalOrExpression) 
		| (QMARK expression COLON expression)
		)?
		;

parenExpr: LPAREN expression RPAREN;

logicalOrExpression: logicalXorExpression (OR logicalXorExpression)*;
logicalXorExpression: logicalAndExpression (XOR logicalAndExpression)*;
logicalAndExpression: relationalExpression (AND relationalExpression)*;
relationalExpression: sumExpr (relationalOperator sumExpr)?;

sumExpr  : prodExpr ((PLUS | MINUS) prodExpr)*;
prodExpr : powExpr ((STAR | DIV | MOD) powExpr)*;
powExpr  : unaryExpression (POWER unaryExpression)*;

unaryExpression 
	:	(PLUS | MINUS | BANG) unaryExpression
	|	primaryExpression
	;

primaryExpression : startNode (node)?;

startNode 
    : 
    (   exprList
    |   parenExpr
    |   methodOrProperty 
    |   functionOrVar 
    |   localFunctionOrVar
    |   reference
    |   literal 
    |   indexer 
    |   type 
    |   constructor
    |   projection 
    |   selection 
    |   firstSelection 
    |   lastSelection 
    |   listInitializer
    |   mapInitializer
    |   lambda
    |   attribute
    )
    ;

node : 
    (   methodOrProperty 
    |   indexer 
    |   projection 
    |   selection 
    |   firstSelection 
    |   lastSelection 
    |   exprList
    |   DOT
    )+
    ;

functionOrVar 
    : function
    | var
    ;

function : POUND ID methodArgs;
var : POUND ID;

localFunctionOrVar 
    : localFunction
    | localVar
    ;

localFunction: DOLLAR ID methodArgs;
localVar: DOLLAR ID;

methodOrProperty
	: method
	| property
	;

method: ID methodArgs;
methodArgs: LPAREN (argument (COMMA argument)*)? RPAREN;

property: ID;

reference
	: (AT LPAREN quotableName COLON)
	| AT LPAREN quotableName RPAREN
	;

indexer: LBRACKET argument (COMMA argument)* RBRACKET;
projection: PROJECT expression RCURLY;
selection:	SELECT expression (COMMA expression)* RCURLY;
firstSelection:	SELECT_FIRST expression RCURLY;
lastSelection: SELECT_LAST expression RCURLY;
type: TYPE name RPAREN;
name: ID (~(RPAREN|COLON|QUOTE))*;
quotableName
    :	STRING_LITERAL
    |	name
    ;
    
attribute: AT LBRACKET qualifiedId (ctorArgs)? RBRACKET;
lambda: LAMBDA (argList)? PIPE expression RCURLY;
argList : (ID (COMMA ID)*);
constructor
	:	'new' qualifiedId ctorArgs
	|   arrayConstructor
	;
arrayConstructor: 'new' qualifiedId arrayRank (listInitializer)?;    
arrayRank: LBRACKET (expression (COMMA expression)*)? RBRACKET;
listInitializer: LCURLY (expression (COMMA expression)*)? RCURLY;
mapInitializer: POUND LCURLY (mapEntry (COMMA mapEntry)*)? RCURLY;
mapEntry: expression COLON expression;
ctorArgs : LPAREN (namedArgument (COMMA namedArgument)*)? RPAREN;
argument : expression;
namedArgument 
    :   ID ASSIGN expression
    |   argument 
    ;
qualifiedId: ID (DOT ID)*;

literal
	:	nullLiteral
	|   integerLiteral
	|   hexLiteral
	|   realLiteral
	|	stringLiteral
	|   boolLiteral
	;

nullLiteral: NULL_LITERAL;
integerLiteral: INTEGER_LITERAL;
hexLiteral: HEXADECIMAL_INTEGER_LITERAL;
realLiteral: REAL_LITERAL;
stringLiteral: STRING_LITERAL;

boolLiteral
    :   TRUE
    |   FALSE
    ;
    
relationalOperator
    :   EQUAL 
    |   NOT_EQUAL
    |   LESS_THAN
    |   LESS_THAN_OR_EQUAL      
    |   GREATER_THAN            
    |   GREATER_THAN_OR_EQUAL 
    |   IN   
    |   IS   
    |   BETWEEN   
    |   LIKE   
    |   MATCHES   
    ; 

WS	:	(' '
	|	'\t'
	|	'\n'
	|	'\r')
	-> skip
	;
	
TRUE: 'true';
FALSE: 'false';
AND: 'and';
OR: 'or';
XOR: 'xor';
IN: 'in';
IS: 'is';
BETWEEN: 'between';
LIKE: 'like';
MATCHES: 'matches';
NULL_LITERAL: 'null';
DOT: '.';

AT: '@';
BACKTICK: '`';
BACKSLASH: '\\';
PIPE: '|';
BANG: '!';
QMARK: '?';
DOLLAR: '$';
POUND: '#';
LPAREN:	'(';
RPAREN:	')';
LBRACKET: '[';
RBRACKET: ']';
LCURLY:	'{';
RCURLY:	'}';
COMMA : ',';
SEMI: ';';
COLON: ':';
ASSIGN: '=';
DEFAULT: '??';
PLUS: '+';
MINUS: '-';
DIV: '/';
STAR: '*';
MOD: '%';
POWER: '^';
EQUAL: '==';
NOT_EQUAL: '!=';
LESS_THAN: '<';
LESS_THAN_OR_EQUAL: '<=';
GREATER_THAN: '>';
GREATER_THAN_OR_EQUAL: '>=';
PROJECT: '!{';
SELECT: '?{';
SELECT_FIRST: '^{';  
SELECT_LAST: '${';
TYPE: 'T(';
LAMBDA: '{|';
DOT_ESCAPED: '\\.'; 
QUOTE: '\'';  
STRING_LITERAL:	QUOTE (APOS|~'\'')* QUOTE;
APOS : QUOTE QUOTE;  
ID: ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'_'|'0'..'9')*;

REAL_LITERAL
    : '.' (DECIMAL_DIGIT)+ (EXPONENT_PART)? (REAL_TYPE_SUFFIX)?
	| (DECIMAL_DIGIT)+ '.' (DECIMAL_DIGIT)+ (EXPONENT_PART)? (REAL_TYPE_SUFFIX)?
	| (DECIMAL_DIGIT)+ (EXPONENT_PART) (REAL_TYPE_SUFFIX)?
	| (DECIMAL_DIGIT)+ (REAL_TYPE_SUFFIX)
	;

INTEGER_LITERAL: (DECIMAL_DIGIT)+ (INTEGER_TYPE_SUFFIX)?;	
HEXADECIMAL_INTEGER_LITERAL: '0x' (HEX_DIGIT)+ (INTEGER_TYPE_SUFFIX)?;
DECIMAL_DIGIT: '0'..'9';	
INTEGER_TYPE_SUFFIX: 'UL' | 'LU' | 'ul' | 'lu' | 'UL' | 'LU' | 'uL' | 'lU' | 'U' | 'L' | 'u' | 'l';		
HEX_DIGIT
	:	'0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 
		'A' | 'B' | 'C' | 'D' | 'E' | 'F'  |
		'a' | 'b' | 'c' | 'd' | 'e' | 'f'
	;	
	
EXPONENT_PART
	:	'e'  (SIGN)*  (DECIMAL_DIGIT)+
	|	'E'  (SIGN)*  (DECIMAL_DIGIT)+
	;	
	
SIGN
	:	'+' | '-'
	;
	
REAL_TYPE_SUFFIX
	: 'F' | 'f' | 'D' | 'd' | 'M' | 'm'	;
