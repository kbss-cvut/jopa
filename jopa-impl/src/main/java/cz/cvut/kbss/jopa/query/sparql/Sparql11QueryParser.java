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

import cz.cvut.kbss.jopa.query.QueryParser;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

/**
 * Grammar-based SPARQL 1.1 query parser.
 * <p>
 * Follows more strictly the standard SPARQL 1.1 grammar with one exception - it allows using variable in property
 * paths, so that they can be set on the query. If they are not set, an exception will be thrown when the query is
 * executed.
 */
public class Sparql11QueryParser implements QueryParser {

    private final ParameterValueFactory parameterValueFactory;

    public Sparql11QueryParser(ParameterValueFactory parameterValueFactory) {
        this.parameterValueFactory = parameterValueFactory;
    }

    @Override
    public TokenStreamSparqlQueryHolder parseQuery(String query) {
        CharStream cs = CharStreams.fromString(query);
        final SparqlLexer lexer = new SparqlLexer(cs);
        final CommonTokenStream tokens = new CommonTokenStream(lexer);
        final SparqlParser s = new SparqlParser(tokens);
        s.addErrorListener(new ParserErrorListener());
        final Sparql11QueryListener listener = new Sparql11QueryListener(parameterValueFactory);

        final ParseTree tree = s.query();
        final ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(listener, tree);

        return new TokenStreamSparqlQueryHolder(query, listener.getQueryAttributes(), listener.getParameters(), tokens);
    }

    @Override
    public TokenStreamSparqlQueryHolder parseQuery(String query, Class<?> resultClass) {
        return parseQuery(query);
    }
}
