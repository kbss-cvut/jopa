grammar soql;


querySentence : selectStatement whereClausuleWrapper? groupByClausule? orderByClausule? ;



selectStatement: typeDef params FROM tables ;

typeDef: SELECT ;

params: paramComma* distinctParam ;

paramComma: distinctParam COMMA ;

distinctParam: distinct? param ;

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



logOp: AND | OR ;



whereClausuleWrapper: WHERE whereClausules ;

whereClausules: whereClausuleOps whereClausuleOps* ;

whereClausuleOps: logOp? NOT? whereClausule ;

whereClausule: whereClausuleParam QUERYOPERATOR whereClausuleValue;

whereClausuleValue: (QMARK TEXT QMARK) | COLONTEXT ;

whereClausuleParam: param | joinedParams ;



orderByClausule: ORDERBY orderByFullFormComma orderByFullFormComma* ;

orderByFullFormComma: orderByFullForm COMMA? ;

orderByFullForm: orderByParam ORDERING? ;

orderByParam: object DOT attribute (DOT attribute)* ;



groupByClausule: GROUPBY groupByParamComma groupByParamComma* ;

groupByParamComma: groupByParam COMMA? ;

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

QUERYOPERATOR: '>' | '<' | '>=' | '<=' | '=' | 'LIKE';

DOT: '.' ;

COMMA: ',' ;

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