package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.jena.util.JenaUtils;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.MultilingualString;
import cz.cvut.kbss.ontodriver.model.Value;
import org.apache.jena.rdf.model.RDFNode;

import java.util.stream.Stream;

class ReferencedListHelper {

    private ReferencedListHelper() {
        throw new AssertionError();
    }

    static Stream<RDFNode> toRdfNodes(Object value, Assertion nodeContentAssertion) {
        if (value instanceof MultilingualString) {
            final MultilingualString mls = (MultilingualString) value;
            return mls.getValue().entrySet().stream()
                      .map((e) -> JenaUtils.valueToRdfNode(nodeContentAssertion, new Value<>(new LangString(e.getValue(), e.getKey()))));
        } else {
            return Stream.of(JenaUtils.valueToRdfNode(nodeContentAssertion, new Value<>(value)));
        }
    }
}
