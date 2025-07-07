package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;

import java.util.List;

public class SimpleListTriplePatternEnhancer extends TriplePatternEnhancer {

    private final ListAttribute<?, ?> attribute;

    public SimpleListTriplePatternEnhancer(ListAttribute<?, ?> attribute) {this.attribute = attribute;}

    @Override
    List<String> getTriplePatterns(String subject, String predicate, String object) {
        return List.of(
                subject + " " + predicate + "/" + IdentifierTransformer.stringifyIri(attribute.getHasNextPropertyIRI()) + "* " + object + " . "
        );
    }
}
