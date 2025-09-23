package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.query.QueryType;

class QueryAttributes {

    boolean hasLimit;
    boolean hasOffset;

    final QueryType queryType;

    QueryAttributes(QueryType queryType) {
        this.queryType = queryType;
    }
}
