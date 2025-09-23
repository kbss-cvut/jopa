package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import org.antlr.v4.runtime.Token;

import java.util.ArrayList;
import java.util.List;

class TokenQueryParameter<T> extends QueryParameter<T> {

    private final List<Token> tokens = new ArrayList<>();

    public TokenQueryParameter(String name, ParameterValueFactory valueFactory) {
        super(name, valueFactory);
    }

    public TokenQueryParameter(Integer position, ParameterValueFactory valueFactory) {
        super(position, valueFactory);
    }

    public List<Token> getTokens() {
        return tokens;
    }
}
