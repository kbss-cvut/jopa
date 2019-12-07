package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.QueryParser;
import cz.cvut.kbss.jopa.query.sparql.SparqlQueryParser;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

public class SoqlQueryParser implements QueryParser {

    private String sparqlQuery;

    @Override
    public QueryHolder parseQuery(String query) {
        CharStream cs = new ANTLRInputStream(query);
        soqlLexer lexer = new soqlLexer(cs);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        soqlParser parser = new soqlParser(tokens);

        ParseTree tree = parser.querySentence();
        SoqlQueryListener listener = new SoqlQueryListener();

        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(listener,tree);

        sparqlQuery = listener.getSoqlQuery();
        return new SparqlQueryParser().parseQuery(sparqlQuery);
    }
}
