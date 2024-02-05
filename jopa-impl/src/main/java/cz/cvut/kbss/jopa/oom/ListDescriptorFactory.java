package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.ListAttribute;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;

/**
 * Creates descriptors for list attributes.
 */
class ListDescriptorFactory {

    static SimpleListDescriptor createSimpleListDescriptor(NamedResource owner, ListAttribute<?, ?> attribute) {
        final boolean inferred = attribute.isInferred();
        final Assertion listProperty = Assertion.createObjectPropertyAssertion(attribute.getIRI().toURI(), inferred);
        final Assertion nextNodeProperty = Assertion.createObjectPropertyAssertion(attribute.getHasNextPropertyIRI()
                                                                                            .toURI(), inferred);
        return new SimpleListDescriptorImpl(owner, listProperty, nextNodeProperty);
    }

    static SimpleListValueDescriptor createSimpleListValueDescriptor(NamedResource owner,
                                                                     ListAttribute<?, ?> attribute) {
        final boolean inferred = attribute.isInferred();
        final Assertion listProperty = Assertion.createObjectPropertyAssertion(attribute.getIRI().toURI(), inferred);
        final Assertion nextNodeProperty = Assertion.createObjectPropertyAssertion(attribute.getHasNextPropertyIRI()
                                                                                            .toURI(), inferred);
        return new SimpleListValueDescriptor(owner, listProperty, nextNodeProperty);
    }

    static ReferencedListDescriptor createReferencedListDescriptor(NamedResource owner, ListAttribute<?, ?> attribute) {
        final boolean inferred = attribute.isInferred();
        final Assertion listProperty = Assertion.createObjectPropertyAssertion(attribute.getIRI().toURI(), inferred);
        final Assertion nextNodeProperty = Assertion
                .createObjectPropertyAssertion(attribute.getHasNextPropertyIRI().toURI(), inferred);
        final Assertion nodeContentProperty = nodeContentProperty(attribute);
        return new ReferencedListDescriptorImpl(owner, listProperty, nextNodeProperty, nodeContentProperty, attribute.isRDFCollection());
    }

    private static Assertion nodeContentProperty(ListAttribute<?, ?> attribute) {
        return attribute.getPersistentAttributeType() == Attribute.PersistentAttributeType.OBJECT
                ? Assertion.createObjectPropertyAssertion(attribute.getHasContentsPropertyIRI()
                                                                   .toURI(), attribute.isInferred())
                : Assertion.createDataPropertyAssertion(attribute.getHasContentsPropertyIRI()
                                                                 .toURI(), attribute.isInferred());
    }

    static <V> ReferencedListValueDescriptor<V> createReferencedListValueDescriptor(NamedResource owner,
                                                                                    ListAttribute<?, ?> attribute) {
        final boolean inferred = attribute.isInferred();
        final Assertion listProperty = Assertion.createObjectPropertyAssertion(attribute.getIRI().toURI(), inferred);
        final Assertion nextNodeProperty = Assertion
                .createObjectPropertyAssertion(attribute.getHasNextPropertyIRI().toURI(), inferred);
        final Assertion nodeContentProperty = nodeContentProperty(attribute);
        return new ReferencedListValueDescriptor<>(owner, listProperty, nextNodeProperty, nodeContentProperty, attribute.isRDFCollection());
    }
}
