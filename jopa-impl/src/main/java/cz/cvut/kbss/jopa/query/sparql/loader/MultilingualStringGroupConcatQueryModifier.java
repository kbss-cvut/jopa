package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Value;

import java.util.stream.Stream;

/**
 * Supports generating GROUP_CONCAT projection for multilingual string attributes.
 */
class MultilingualStringGroupConcatQueryModifier extends GroupConcatQueryModifier {

    MultilingualStringGroupConcatQueryModifier(QueryVariableMapping variableMapping) {
        super(variableMapping);
    }

    @Override
    String generateGroupConcat() {
        // (GROUP_CONCAT(DISTINCT CONCAT(STR(?var), "@", LANG(?var)); SEPARATOR='') AS ?var_gc)
        final String queryVar = "?" + variableMapping.attributeVar();
        return "(GROUP_CONCAT(DISTINCT CONCAT('\"', STR(" + queryVar + "), '\"@', " +
                "LANG(" + queryVar + ")); SEPARATOR='" + GROUP_CONCAT_SEPARATOR + "') " +
                "AS " + queryVar + GROUP_CONCAT_SUFFIX + ")";
    }

    @Override
    Stream<Value<?>> readValue(ResultRow row) throws OntoDriverException {
        assert row.isBound(variableMapping.attributeVar());
        final String values = row.getString(variableMapping.attributeVar());
        if (values.isEmpty()) {
            return Stream.empty();
        }
        return Stream.of(values.split(GROUP_CONCAT_SEPARATOR))
                     .map(MultilingualStringGroupConcatQueryModifier::toValue);
    }

    private static Value<?> toValue(String token) {
        // The token is produced by CONCAT(STR(?var), "@", LANG(?var)).
        // The lexical value may itself contain '@', so split on the last occurrence.
        final int sep = token.lastIndexOf('@');
        if (sep < 0) {
            return new Value<>(token);
        }
        final String value = token.substring(1, sep - 1);
        final String language = token.substring(sep + 1);
        return new Value<>(language.isEmpty() ? value : new LangString(value, language));
    }
}
