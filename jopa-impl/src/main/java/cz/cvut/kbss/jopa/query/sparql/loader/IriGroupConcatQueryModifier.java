package cz.cvut.kbss.jopa.query.sparql.loader;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;
import java.util.stream.Stream;

/**
 * Supports modifying a query to project GROUP_CONCAT for a plural attribute whose values are identifiers, and for a
 * types attributes.
 */
class IriGroupConcatQueryModifier extends GroupConcatQueryModifier {

    IriGroupConcatQueryModifier(QueryVariableMapping variableMapping) {
        super(variableMapping);
    }

    @Override
    String generateGroupConcat() {
        this.triggered = true;
        return "(GROUP_CONCAT(DISTINCT ?" + variableMapping.attributeVar() + "; SEPARATOR='" +
                GROUP_CONCAT_SEPARATOR + "') AS ?" + variableMapping.attributeVar() + GROUP_CONCAT_SUFFIX + ")";
    }

    @Override
    Stream<Value<?>> readValue(ResultRow row) throws OntoDriverException {
        assert row.isBound(variableMapping.attributeVar());
        final String values = row.getString(variableMapping.attributeVar());
        // This does not handle blank nodes
        return Stream.of(values.split(AttributeEnumeratingSparqlAssemblyModifier.GROUP_CONCAT_SEPARATOR))
                     .map(URI::create).map(Value::new);
    }
}
