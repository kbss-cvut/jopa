grammar Soql;

start: querySentence EOF ;

querySentence: selectStatement ;

selectStatement: selectClause fromClause whereClause? groupByClause? orderByClause? ;

objectPathExpression: simplePath DOT objectField ;

simplePath: objectField (DOT simplePath)* ;

objectField: IDENTIFICATION_VARIABLE ;

selectClause: SELECT (DISTINCT)? selectItem (',' selectItem)* ;

selectItem: selectExpression;

selectExpression: simplePath | aggregateExpression ;

aggregateExpression: COUNT '(' (DISTINCT)? simplePath ')';

fromClause: FROM entityName IDENTIFICATION_VARIABLE;

entityName: IDENTIFICATION_VARIABLE ;

whereClause
    : WHERE conditionalExpression
    ;

conditionalExpression
    : (conditionalTerm) (OR conditionalTerm)*
    ;

conditionalTerm
   : (conditionalFactor) (AND conditionalFactor)*
   ;

conditionalFactor
   : (NOT)? simpleConditionalExpression
   ;

simpleConditionalExpression
   : comparisonExpression
   | likeExpression
   | inExpression
   | memberOfExpression
   ;

inExpression
   : simplePath (NOT)? IN '('? (inItem (',' inItem)*) ')'?
   ;

inItem
   : literal
   | inputParameter
   ;

literal
   : STRING_LITERAL
   | INT_LITERAL
   | FLOAT_LITERAL
   | BOOLEAN_LITERAL
   | IDENTIFICATION_VARIABLE
   ;

likeExpression
   : stringExpression (NOT)? LIKE stringExpression
   ;

memberOfExpression
    : inItem (NOT)? MEMBER OF simplePath
    ;

entityExpression
    : IDENTIFICATION_VARIABLE
    | inputParameter
    ;

comparisonExpression
   : stringExpression comparisonOperator stringExpression
   | simpleArithmeticExpression comparisonOperator simpleArithmeticExpression
   | entityExpression op=(EQUAL | NOT_EQUAL) ( entityExpression )
   ;

stringExpression
   : simplePath
   | STRING_LITERAL
   | inputParameter
   | functionsReturningStrings
   ;

functionsReturningStrings
   : 'CONCAT' '(' stringExpression ',' stringExpression ')'
   | 'SUBSTRING' '(' stringExpression ',' simpleArithmeticExpression ',' simpleArithmeticExpression ')'
   | 'LOWER' '(' stringExpression ')'
   | 'UPPER' '(' stringExpression ')'
   | 'LANG' '(' simplePath ')'
   ;

simpleArithmeticExpression
   : (arithmeticTerm) (('+' | '-') arithmeticTerm)*
   ;

arithmeticTerm
   : (arithmeticFactor) (('*' | '/') arithmeticFactor)*
   ;

arithmeticFactor
   : ('+' | '-')? arithmeticPrimary
   ;

arithmeticPrimary
   : simplePath
   | literal
   | '(' simpleArithmeticExpression ')'
   | inputParameter
   | functionsReturningNumerics
   ;

functionsReturningNumerics
   : 'LENGTH' '(' stringExpression ')'
   | 'ABS' '(' simpleArithmeticExpression ')'
   | 'ROUND' '(' simpleArithmeticExpression ')'
   | 'CEIL' '(' simpleArithmeticExpression ')'
   | 'FLOOR' '(' simpleArithmeticExpression ')'
   ;

orderByClause: ORDER BY orderByItem (',' orderByItem)* ;

orderByItem: objectPathExpression (ASC | DESC) ;

groupByClause: GROUP BY groupByItem (',' groupByItem)* ;

groupByItem: objectPathExpression ;

inputParameter: COLON IDENTIFICATION_VARIABLE ;

comparisonOperator
    : op=EQUAL
    | op='>'
    | op='>='
    | op='<'
    | op='<='
    | op=NOT_EQUAL
    ;

SELECT: 'SELECT' ;

WHERE: 'WHERE' ;

NOT: 'NOT' ;

FROM: 'FROM' ;

JOIN: 'JOIN' ;

AND: 'AND' ;

OR: 'OR' ;

BY: 'BY' ;

OF: 'OF' ;

ORDER: 'ORDER' ;

GROUP: 'GROUP' ;

ASC: 'ASC' ;

DESC: 'DESC' ;

DISTINCT: 'DISTINCT' ;

COUNT: 'COUNT' ;

LIKE: 'LIKE' ;

IN: 'IN' ;

MEMBER: 'MEMBER' ;

EQUAL: '=' ;
NOT_EQUAL: '<>' | '!=' ;

DOT: '.' ;

QMARK: '"' ;

COLON: ':' ;

TRUE: 'TRUE';

FALSE: 'FALSE';

IDENTIFICATION_VARIABLE: (LOWERCASE | UPPERCASE | '_') (LOWERCASE | UPPERCASE | DIGIT | '_')* ;

STRING_LITERAL: QMARK TEXT QMARK ;

INT_LITERAL: DIGIT+;

FLOAT_LITERAL: DIGIT* '.' DIGIT;

BOOLEAN_LITERAL: TRUE | FALSE;

TEXT: (LOWERCASE | UPPERCASE | DIGIT)+ ;

UPPERCASE: ('A'..'Z');

LOWERCASE: ('a'..'z');

DIGIT: ('0'..'9');

WHITESPACE: (' ')+ -> skip;
