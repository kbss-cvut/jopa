package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.model.JOPAExperimentalProperties;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.QueryParser;
import cz.cvut.kbss.jopa.query.QueryType;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import cz.cvut.kbss.jopa.utils.Configuration;
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
    private final Metamodel metamodel;
    private final Configuration configuration;

    public Sparql11QueryParser(ParameterValueFactory parameterValueFactory, Metamodel metamodel,
                               Configuration configuration) {
        this.parameterValueFactory = parameterValueFactory;
        this.metamodel = metamodel;
        this.configuration = configuration;
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
        if (query.getQueryType() != QueryType.SELECT || !isResultClassEntity(resultClass)
                || !isSingleVariableProjected(query) || !configuration.is(JOPAExperimentalProperties.QUERY_ENABLE_ENTITY_LOADING_OPTIMIZER)) {
            return;
        }
        final EntityLoadingOptimizer optimizer = new EntityLoadingOptimizer();
        query.addAssemblyModifier(optimizer);
    }

    private boolean isResultClassEntity(Class<?> resultClass) {
        return metamodel.getEntities().stream().anyMatch(et -> et.getBindableJavaType().equals(resultClass));
    }

    private static boolean isSingleVariableProjected(TokenStreamSparqlQueryHolder query) {
        return query.getQueryParameters().stream().filter(QueryParameter::isProjected).count() == 1;
    }
}
