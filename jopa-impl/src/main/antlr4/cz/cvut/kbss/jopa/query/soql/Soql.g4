grammar Soql;


querySentence : selectStatement whereClauseWrapper? groupByClause? orderByClause? ;

selectStatement: typeDef params FROM tables ;

typeDef: SELECT ;

params: paramComma* distinctParam ;

paramComma: distinctParam ',' ;

distinctParam: distinct? selectedParam ;

selectedParam: param | count;

count: COUNT '(' param ')' ;

param: objWithAttr | objWithOutAttr ;

objWithAttr: object DOT attribute;

objWithOutAttr: object ;

distinct: DISTINCT ;

object: IDENTIFICATION_VARIABLE ;

attribute: IDENTIFICATION_VARIABLE ;

joinedParams: object DOT attribute (DOT attribute)+ ;



tables: tableWithName ;

table: IDENTIFICATION_VARIABLE ;

tableName: IDENTIFICATION_VARIABLE ;

tableWithName: table tableName ;


whereClauseWrapper
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
   : whereClauseParam ('NOT')? LIKE whereClauseValue
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
