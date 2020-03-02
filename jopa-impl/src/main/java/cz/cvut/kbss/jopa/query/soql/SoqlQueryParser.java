package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.QueryParser;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

public class SoqlQueryParser implements QueryParser {

    private final QueryParser sparqlParser;
    private final MetamodelImpl metamodel;

    public SoqlQueryParser(QueryParser sparqlParser, MetamodelImpl metamodel) {
        this.sparqlParser = sparqlParser;
        this.metamodel = metamodel;
    }


    @Override
    public QueryHolder parseQuery(String query) {
        CharStream cs = CharStreams.fromString(query);
        SoqlLexer lexer = new SoqlLexer(cs);
        final CommonTokenStream tokens = new CommonTokenStream(lexer);
        SoqlParser parser = new SoqlParser(tokens);

        final ParseTree tree = parser.querySentence();
        final SoqlQueryListener listener = new SoqlQueryListener(this.metamodel);

        final ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(listener, tree);

        return sparqlParser.parseQuery(listener.getSoqlQuery());
    }
}
