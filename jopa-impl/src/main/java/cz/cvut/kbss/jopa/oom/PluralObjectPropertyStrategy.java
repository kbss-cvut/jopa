package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.ontodriver.exceptions.NotYetImplementedException;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;

abstract class PluralObjectPropertyStrategy<X> extends FieldStrategy<Attribute<? super X, ?>, X> {

    final PluralAttribute<? super X, ?, ?> pluralAtt;
    private Collection<Object> values;

    public PluralObjectPropertyStrategy(EntityType<X> et, Attribute<? super X, ?> att,
                                        Descriptor descriptor, EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
        this.pluralAtt = (PluralAttribute<? super X, ?, ?>) attribute;
        initCollection();
    }

    private void initCollection() {
        switch (pluralAtt.getCollectionType()) {
            case LIST:
                this.values = new ArrayList<>();
                break;
            case COLLECTION:
            case SET:
                this.values = new HashSet<>();
                break;
            default:
                throw new NotYetImplementedException("This type of collection is not supported yet.");
        }
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        final NamedResource valueIdentifier = (NamedResource) ax.getValue().getValue();
        final Object value = mapper.getEntityFromCacheOrOntology(pluralAtt.getBindableJavaType(),
                valueIdentifier.getIdentifier(), attributeDescriptor);
        values.add(value);

    }

    @Override
    void buildInstanceFieldValue(Object instance) throws IllegalArgumentException,
            IllegalAccessException {
        validateCardinalityConstraints(values.size());
        setValueOnInstance(instance, values);
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createObjectPropertyAssertion(pluralAtt.getIRI().toURI(),
                attribute.isInferred());
    }
}
