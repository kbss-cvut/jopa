grammar soql;


querySentence : typeDef distinct? params FROM tables WHERE? whereClausuleJoin? whereClausules? orderByClausule?;



params: paramComma* param ;

param: objWithAttr | objWithOutAttr ;

joinedParams: object DOT attribute (DOT attribute)+ ;

paramComma: param COMMA ;

object: TEXT ;

objWithAttr: object DOT attribute;

objWithOutAttr: object ;

attribute: TEXT ;



typeDef: SELECT ;

distinct: DISTINCT ;



logOp: AND | OR ;



tables: tableWithName ;

table: TEXT ;

tableName: TEXT ;

tableWithName: table tableName ;



whereClausules: whereClausuleNot whereClausuleNot* ;

whereClausuleNot: logOp? NOT? whereClausule ;

whereClausule: param QUERYOPERATOR whereClausuleValue;

whereClausuleJoin: clausuleJoinNot clausuleJoinNot* ;

whereClausuleValue: (QMARK TEXT QMARK) | COLONTEXT ;

clausuleJoinNot : logOp? NOT? clausuleJoin ;

clausuleJoin: joinedParams QUERYOPERATOR whereClausuleValue ;



orderByClausule: ORDERBY orderBySingleComma orderBySingleComma* ;

orderBySingleComma: orderBySingle COMMA? ;

orderBySingle: orderByParam ORDERING? ;

orderByParam: object DOT attribute (DOT attribute)* ;



SELECT: 'SELECT' | 'select' | 'Select' ;

WHERE: 'WHERE' | 'where' | 'Where' ;

NOT: 'NOT' | 'not' | 'Not' ;

FROM: 'FROM' | 'from' | 'From' ;

JOIN: 'JOIN' | 'join' | 'Join' ;

AND: 'AND' | 'and' | 'And' ;

OR: 'OR' | 'or' | 'Or' ;

ORDERBY: 'ORDER BY' ;

ORDERING: ASC | DESC ;

ASC: 'ASC' ;

DESC: 'DESC' ;

DISTINCT: 'DISTINCT' ;

LEFTOUTERJOIN: 'LEFT OUTER JOIN' | 'left outer join' | 'Left Outer Join' ;

QUERYOPERATOR: '>' | '<' | '>=' | '<=' | '=' | 'LIKE';

RIGHTPAREN: ')' ;

LEFTPAREN: '(' ;

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