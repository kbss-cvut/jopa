package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.Value;

class IdentifierFieldStrategy<X> extends FieldStrategy<Identifier<? super X, ?>, X> {

    IdentifierFieldStrategy(EntityType<X> et, Identifier<? super X, ?> att, Descriptor attributeDescriptor,
                            EntityMappingHelper mapper) {
        super(et, att, attributeDescriptor, mapper);
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        // Do nothing
    }

    @Override
    void buildInstanceFieldValue(Object instance) throws IllegalAccessException {
        // Do nothing
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) throws IllegalAccessException {
        valueBuilder.addValue(createAssertion(), new Value<>(et.getIRI().toURI()), attributeDescriptor.getContext());
    }

    @Override
    Assertion createAssertion() {
        assert !attribute.isInferred();
        return Assertion.createClassAssertion(attribute.isInferred());
    }
}
