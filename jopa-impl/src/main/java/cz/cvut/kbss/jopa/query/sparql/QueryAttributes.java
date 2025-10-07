package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.query.QueryType;
import org.antlr.v4.runtime.Token;

public record QueryAttributes(QueryType queryType, boolean hasOffset, boolean hasLimit, boolean hasGraphOrService, Token lastClosingCurlyBraceToken) {
}
