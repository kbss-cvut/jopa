grammar Soql;

start: querySentence EOF ;

querySentence: selectStatement ;

selectStatement: selectClause fromClause whereClause? groupByClause? orderByClause? ;

selectClause: SELECT (DISTINCT)? selectItem (',' selectItem)* ;

selectItem: selectExpression;

selectExpression: param | count ;

param: objWithAttr | objWithOutAttr ;

objWithAttr: object DOT attribute;

objWithOutAttr: object ;

object: IDENTIFICATION_VARIABLE ;

count: COUNT '(' param ')' ;

attribute: IDENTIFICATION_VARIABLE ;

joinedParams: object DOT attribute (DOT attribute)+ ;

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
   : whereClauseParam (NOT)? IN '('? (inItem (',' inItem)*) ')'?
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
    : inItem (NOT)? MEMBEROF whereClauseParam
    ;

comparisonExpression
   : stringExpression COMPARISON_OPERATOR stringExpression
   | simpleArithmeticExpression COMPARISON_OPERATOR simpleArithmeticExpression
   | whereClauseParam COMPARISON_OPERATOR ( whereClauseParam | whereClauseValue )
   ;

whereClauseValue: (QMARK TEXT QMARK) | inputParameter ;

whereClauseParam: param | joinedParams ;

stringExpression
   : whereClauseParam
   | inputParameter
   | functionsReturningStrings
   ;

functionsReturningStrings
   : 'CONCAT' '(' stringExpression ',' stringExpression ')'
   | 'SUBSTRING' '(' stringExpression ',' simpleArithmeticExpression ',' simpleArithmeticExpression ')'
   | 'LOWER' '(' stringExpression ')'
   | 'UPPER' '(' stringExpression ')'
   | 'LANG' '(' whereClauseParam ')'
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
   : param
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

orderByClause: ORDERBY orderByFullFormComma orderByFullFormComma* ;

orderByFullFormComma: orderByFullForm ','? ;

orderByFullForm: orderByParam ORDERING? ;

orderByParam: object DOT attribute (DOT attribute)* ;

groupByClause: GROUPBY groupByParamComma groupByParamComma* ;

groupByParamComma: groupByParam ','? ;

groupByParam: object DOT attribute (DOT attribute)* ;

inputParameter: COLON IDENTIFICATION_VARIABLE ;


SELECT: 'SELECT' ;

WHERE: 'WHERE' ;

NOT: 'NOT' ;

FROM: 'FROM' ;

JOIN: 'JOIN' ;

AND: 'AND' ;

OR: 'OR' ;

ORDERBY: 'ORDER BY' ;

ORDERING: ASC | DESC ;

GROUPBY: 'GROUP BY' ;

ASC: 'ASC' ;

DESC: 'DESC' ;

DISTINCT: 'DISTINCT' ;

COUNT: 'COUNT' ;

LIKE: 'LIKE' ;

IN: 'IN' ;

MEMBEROF: 'MEMBER OF' ;

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
