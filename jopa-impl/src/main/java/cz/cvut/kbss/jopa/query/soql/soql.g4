grammar soql;


querySentence : typeDef params FROM tables WHERE? whereClausuleJoin? whereClausules?;



params: paramComma* param ;

param: objWithAttr | objWithOutAttr ;

joinedParams: object DOT attribute (DOT attribute)+ ;

paramComma: param COMMA ;

object: TEXT ;

objWithAttr: object DOT attribute;

objWithOutAttr: object ;

attribute: TEXT ;



typeDef: SELECT ;



logOp: AND | OR ;



tables: tableWithName ;

table: TEXT ;

tableName: TEXT ;

tableWithName: table tableName ;



whereClausules: whereClausuleNot* ;

whereClausuleNot: NOT? whereClausule ;

whereClausule: param QUERYOPERATOR whereClausuleValue logOp?;

whereClausuleJoin: clausuleJoinNot* ;

whereClausuleValue: (QMARK TEXT QMARK) | COLONTEXT ;

// whereClausuleMultiple: whereClausule logOp ;


clausuleJoinNot : NOT? clausuleJoin ;
clausuleJoin: joinedParams QUERYOPERATOR whereClausuleValue logOp? ;


/*

joinClausule: JOIN joinTable ;

leftOuterJoinClausule: LEFTOUTERJOIN joinTable ;

joinTable: tableWithName DOT tableParam joinParam ;

tableParam: TEXT ;

joinParam: TEXT ;

*/


SELECT: 'SELECT' | 'select' | 'Select' ;

WHERE: 'WHERE' | 'where' | 'Where' ;

NOT: 'NOT' | 'not' | 'Not' ;

FROM: 'FROM' | 'from' | 'From' ;

JOIN: 'JOIN' | 'join' | 'Join' ;

AND: 'AND' | 'and' | 'And' ;

OR: 'OR' | 'or' | 'Or' ;

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