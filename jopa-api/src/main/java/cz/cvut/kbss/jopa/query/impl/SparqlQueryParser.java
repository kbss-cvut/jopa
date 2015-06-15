package cz.cvut.kbss.jopa.query.impl;

import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.query.QueryParser;

import java.util.ArrayList;
import java.util.List;

/**
 * @author kidney
 */
public class SparqlQueryParser implements QueryParser {

    @Override
    public QueryHolder parseQuery(String query) {
        final List<String> queryParts = new ArrayList<>();
        final List<String> parameters = new ArrayList<>();
        boolean inSQString = false;
        // In double-quoted string
        boolean inDQString = false;
        boolean inParam = false;
        int lastParamEndIndex = 0;
        int paramStartIndex = 0;
        for (int i = 0; i < query.length(); i++) {
            final char c = query.charAt(i);
            switch (c) {
                case '\'':
                    inSQString = !inSQString;
                    break;
                case '"':
                    inDQString = !inDQString;
                    break;
                case '?':
                    if (!inSQString && !inDQString) {
                        queryParts.add(query.substring(lastParamEndIndex, i));
                        paramStartIndex = i + 1;
                        inParam = true;
                    }
                    break;
                // TODO Take a look at some existing SPARQL parsers and maybe use them instead of this simplified version
                case '<':
                case '>':
                case ',':
                case '\n':
                case ' ':
                    if (inParam) {
                        lastParamEndIndex = i;
                        inParam = false;
                        final String param = query.substring(paramStartIndex, i);
                        parameters.add(param);
                    }
                    break;
                default:
                    break;
            }
        }
        queryParts.add(query.substring(lastParamEndIndex));
        return new SparqlQueryHolder(queryParts, parameters);
    }
}
