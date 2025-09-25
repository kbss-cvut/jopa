package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.query.QueryParser;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Grammar-based SPARQL 1.1 query parser.
 * <p>
 * Follows more strictly the standard SPARQL 1.1 grammar with one exception - it allows using variable in property
 * paths, so that they can be set on the query. If they are not set, an exception will be thrown when the query is
 * executed.
 */
public class Sparql11QueryParser implements QueryParser {

    private static final Logger LOG = LoggerFactory.getLogger(Sparql11QueryParser.class);

    private final ParameterValueFactory parameterValueFactory;
    private final EntityLoadingOptimizer optimizer;

    public Sparql11QueryParser(ParameterValueFactory parameterValueFactory, EntityLoadingOptimizer optimizer) {
        this.parameterValueFactory = parameterValueFactory;
        this.optimizer = optimizer;
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
        final TokenStreamSparqlQueryHolder holder = parseQuery(query);
        optimizeQueryIfPossible(holder, resultClass);
        return holder;
    }

    private void optimizeQueryIfPossible(TokenStreamSparqlQueryHolder query, Class<?> resultClass) {
        optimizer.getSparqlAssemblyModifier(query, resultClass).ifPresent(modifier -> {
            LOG.trace("Using entity loading optimizer to enhance query.");
            query.addAssemblyModifier(modifier);
        });
    }
}
