package cz.cvut.kbss.jopa.query.soql;

import cz.cvut.kbss.jopa.model.annotations.SequenceType;
import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;

import java.util.List;

/**
 * Enhances triple patterns if necessary.
 * <p>
 * For example, an RDF list enhancer may add triple patterns for getting list elements.
 */
class TriplePatternEnhancer {

    /**
     * Gets (possibly enhanced) triple patterns for the specified subject, predicate, and object.
     *
     * @param subject   Subject position value
     * @param predicate Predicate position value
     * @param object    Object position value
     * @return Triple patterns
     */
    List<String> getTriplePatterns(String subject, String predicate, String object) {
        return List.of(subject + " " + predicate + " " + object + " . ");
    }

    /**
     * Creates the appropriate triple pattern enhancer for the specified attribute.
     *
     * @param attribute Attribute to get enhancer for
     * @return Triple pattern enhancer
     */
    static TriplePatternEnhancer create(FieldSpecification<?, ?> attribute) {
        if (attribute.isMappedAttribute() && attribute.isCollection()) {
            final PluralAttribute<?, ?, ?> pluralAttribute = (PluralAttribute<?, ?, ?>) attribute;
            if (pluralAttribute.isRdfContainer()) {
                return new RdfContainerTriplePatternEnhancer();
            }
            if (pluralAttribute.isRDFCollection()) {
                return new RdfCollectionTriplePatternEnhancer();
            }
            if (pluralAttribute.getCollectionType() == CollectionType.LIST) {
                final ListAttribute<?, ?> listAttribute = (ListAttribute<?, ?>) pluralAttribute;
                if (listAttribute.getSequenceType() == SequenceType.simple) {
                    return new SimpleListTriplePatternEnhancer(listAttribute);
                } else {
                    assert listAttribute.getSequenceType() == SequenceType.referenced;
                    return new ReferencedListTriplePatterEnhancer(listAttribute);
                }
            }
        }

        return new TriplePatternEnhancer();
    }
}
