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

fromClause: FROM entityName identificationVariable;

entityName: IDENTIFICATION_VARIABLE ;

identificationVariable: IDENTIFICATION_VARIABLE ;

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
   | whereClauseValue
   ;

literal
   :
   ;

likeExpression
   : stringExpression (NOT)? LIKE whereClauseValue
   ;

memberOfExpression
    : inItem (NOT)? MEMBER OF simplePath
    ;

comparisonExpression
   : stringExpression COMPARISON_OPERATOR stringExpression
   | simpleArithmeticExpression COMPARISON_OPERATOR simpleArithmeticExpression
   | simplePath COMPARISON_OPERATOR ( simplePath | whereClauseValue )
   ;

whereClauseValue: (QMARK TEXT QMARK) | inputParameter ;

stringExpression
   : simplePath
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

COMPARISON_OPERATOR: '>' | '<' | '>=' | '<=' | '=' | '<>' | '!=' ;

DOT: '.' ;

QMARK: '"' ;

COLON: ':' ;

IDENTIFICATION_VARIABLE: (LOWERCASE | UPPERCASE | '_') (LOWERCASE | UPPERCASE | DIGIT | '_')* ;

TEXT: (LOWERCASE | UPPERCASE | DIGIT)+ ;

UPPERCASE: ('A'..'Z');

LOWERCASE: ('a'..'z');

DIGIT: ('0'..'9');

NUMBER: DIGIT+ ;

VALUE: NUMBER ;

WHITESPACE: (' ')+ -> skip;
