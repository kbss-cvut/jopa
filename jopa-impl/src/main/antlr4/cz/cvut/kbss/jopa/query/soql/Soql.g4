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

object: TEXT ;

attribute: TEXT ;

joinedParams: object DOT attribute (DOT attribute)+ ;



tables: tableWithName ;

table: TEXT ;

tableName: TEXT ;

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

comparisonExpression: whereClauseParam COMPARISON_OPERATOR whereClauseValue;

whereClauseValue: (QMARK TEXT QMARK) | COLONTEXT ;

whereClauseParam: param | joinedParams ;

orderByClause: ORDERBY orderByFullFormComma orderByFullFormComma* ;

orderByFullFormComma: orderByFullForm ','? ;

orderByFullForm: orderByParam ORDERING? ;

orderByParam: object DOT attribute (DOT attribute)* ;

groupByClause: GROUPBY groupByParamComma groupByParamComma* ;

groupByParamComma: groupByParam ','? ;

groupByParam: object DOT attribute (DOT attribute)* ;



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

TEXT: (LOWERCASE | UPPERCASE | DIGIT)+ ;

COLONTEXT: COLON TEXT ;

UPPERCASE: ('A'..'Z');

LOWERCASE: ('a'..'z');

DIGIT: ('0'..'9');

NUMBER: DIGIT+ ;

VALUE: NUMBER ;

WHITESPACE: (' ')+ -> skip;
