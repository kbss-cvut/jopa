package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.exception.QueryParserException;
import cz.cvut.kbss.jopa.query.QueryParser;
import cz.cvut.kbss.jopa.query.parameter.ParameterValueFactory;
import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.atn.ATNConfigSet;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import java.util.BitSet;

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
        s.addErrorListener(new ErrorListener());
        final Sparql11QueryListener listener = new Sparql11QueryListener(parameterValueFactory);

        final ParseTree tree = s.query();
        final ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(listener, tree);

        return new TokenStreamSparqlQueryHolder(query, listener.getQueryAttributes(), listener.getParameters(), tokens);
    }

    private static class ErrorListener implements ANTLRErrorListener {

        @Override
        public void syntaxError(Recognizer<?, ?> recognizer, Object o, int i, int i1, String s,
                                RecognitionException e) {
            throw new QueryParserException("Invalid SPARQL query syntax.", e);
        }

        @Override
        public void reportAmbiguity(Parser parser, DFA dfa, int i, int i1, boolean b, BitSet bitSet,
                                    ATNConfigSet atnConfigSet) {
            // Do nothing
        }

        @Override
        public void reportAttemptingFullContext(Parser parser, DFA dfa, int i, int i1, BitSet bitSet,
                                                ATNConfigSet atnConfigSet) {
            // Do nothing
        }

        @Override
        public void reportContextSensitivity(Parser parser, DFA dfa, int i, int i1, int i2, ATNConfigSet atnConfigSet) {
            // Do nothing
        }
    }
}
