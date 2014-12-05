package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.oom.exceptions.EntityDeconstructionException;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.Value;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;

public class TypesFieldStrategy<X> extends FieldStrategy<TypesSpecification<? super X, ?>, X> {

    private final Set<String> values = new HashSet<>();

    public TypesFieldStrategy(EntityType<X> et, TypesSpecification<? super X, ?> att,
                              Descriptor descriptor, EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        if (MappingUtils.isEntityClassAssertion(ax, et)) {
            return;
        }
        final String typeAsString = ax.getValue().stringValue();
        values.add(typeAsString);
    }

    @Override
    void buildInstanceFieldValue(Object instance) throws IllegalArgumentException,
            IllegalAccessException {
        assert attribute.getJavaField().getType().isAssignableFrom(Set.class);

        if (values.isEmpty()) {
            return;
        }
        setValueOnInstance(instance, values);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder)
            throws IllegalArgumentException, IllegalAccessException {
        final Object val = extractFieldValueFromInstance(instance);
        if (val == null) {
            valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeContext());
            return;
        }
        if (!(val instanceof Set)) {
            throw new EntityDeconstructionException(
                    "The types field is not of a valid type. Expected Set<String>.");
        }
        final Set<?> types = (Set<?>) val;
        if (types.isEmpty()) {
            return;
        }
        final Set<URI> values = new HashSet<>();
        for (Object type : types) {
            try {
                values.add(URI.create(type.toString()));
            } catch (IllegalArgumentException e) {
                throw new EntityDeconstructionException("Type " + type + " is not a valid URI.", e);
            }

        }
        // If we're updating types in the same context as the individual, we must make sure that the entity class assertion stays there
        if (shouldAddEntityClassAssertion(valueBuilder)) {
            values.add(et.getIRI().toURI());
        }
        valueBuilder.addTypes(values, getAttributeContext());
    }

    private boolean shouldAddEntityClassAssertion(AxiomValueGatherer valueBuilder) {
        return !valueBuilder.containsClassAssertion() && areTypesInSubjectContext(valueBuilder.getEntityContext(), getAttributeContext());
    }

    private boolean areTypesInSubjectContext(URI subjectContext, URI typesContext) {
        if (subjectContext == null) {
            return typesContext == null;
        } else {
            return typesContext != null && subjectContext.equals(typesContext);
        }
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createClassAssertion(attribute.isInferred());
    }
}
