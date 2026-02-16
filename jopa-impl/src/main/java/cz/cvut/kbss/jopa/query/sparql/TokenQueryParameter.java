/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import org.antlr.v4.runtime.Token;

import java.util.ArrayList;
import java.util.List;

/**
 * Query parameter represented by ANTLR tokens in the query string.
 *
 * @param <T> Parameter value type
 */
public class TokenQueryParameter<T> extends QueryParameter<T> {

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

    public Token getSingleToken() {
        return tokens.get(0);
    }
}
