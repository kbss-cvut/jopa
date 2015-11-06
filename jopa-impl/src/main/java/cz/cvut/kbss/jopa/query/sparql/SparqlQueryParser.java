package cz.cvut.kbss.jopa.query.sparql;

import cz.cvut.kbss.jopa.query.QueryParameter;
import cz.cvut.kbss.jopa.query.QueryParser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SparqlQueryParser implements QueryParser {

    private Map<String, QueryParameter<?>> uniqueParams;

    @Override
    public SparqlQueryHolder parseQuery(String query) {
        final List<String> queryParts = new ArrayList<>();
        this.uniqueParams = new HashMap<>();
        final List<QueryParameter<?>> parameters = new ArrayList<>();
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
                // TODO Use $ for position parameters
                case '<':
                case '>':
                case ',':
                case '\n':
                case ' ':
                    if (inParam) {
                        lastParamEndIndex = i;
                        inParam = false;
                        final String param = query.substring(paramStartIndex, i);
                        parameters.add(getQueryParameter(param));
                    }
                    break;
                default:
                    break;
            }
        }
        queryParts.add(query.substring(lastParamEndIndex));
        return new SparqlQueryHolder(query, queryParts, parameters);
    }

    private QueryParameter<?> getQueryParameter(String name) {
        // We want to reuse the param instances, so that changes to them apply throughout the whole query
        if (!uniqueParams.containsKey(name)) {
            uniqueParams.put(name, new QueryParameter<>(name));
        }
        return uniqueParams.get(name);
    }
}
