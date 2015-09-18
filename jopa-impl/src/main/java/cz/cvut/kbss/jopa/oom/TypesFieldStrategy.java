package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

import java.net.URI;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

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
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) throws IllegalAccessException {
        final Object val = extractFieldValueFromInstance(instance);
        final X original = mapper.getOriginalInstance(instance);
        if (val == null) {
            if (original == null) {
                return;
            }
            final Set<?> origTypes = (Set<?>) extractFieldValueFromInstance(original);
            if (origTypes == null || origTypes.isEmpty()) {
                return;
            }
            valueBuilder.removeTypes(prepareTypes(origTypes), getAttributeContext());
        } else {
            assert val instanceof Set;  // This is verified when the metamodel is built
            final Set<?> types = (Set<?>) val;
            if (original == null) {
                valueBuilder.addTypes(prepareTypes(types), getAttributeContext());
            } else {
                final Set<?> origTypes = (Set<?>) extractFieldValueFromInstance(original);
                extractTypesToAdd(valueBuilder, types, origTypes);
                extractTypesToRemove(valueBuilder, types, origTypes);
            }
        }
    }

    private void extractTypesToAdd(AxiomValueGatherer valueBuilder, Set<?> types, Set<?> origTypes) {
        final Set<URI> toAdd = typesDiff(origTypes, types);
        valueBuilder.addTypes(toAdd, getAttributeContext());
    }

    private Set<URI> typesDiff(Set<?> base, Set<?> difference) {
        final Set<URI> addedDiff = new HashSet<>(base.size());
        addedDiff.addAll(difference.stream().filter(t -> !base.contains(t)).map(t -> URI.create(t.toString()))
                                .collect(Collectors.toList()));
        return addedDiff;
    }

    private void extractTypesToRemove(AxiomValueGatherer valueBuilder, Set<?> types, Set<?> origTypes) {
        final Set<URI> toRemove = typesDiff(types, origTypes);
        valueBuilder.removeTypes(toRemove, getAttributeContext());
    }

    private Set<URI> prepareTypes(Set<?> types) {
        final Set<URI> toAdd = new HashSet<>(types.size());
        toAdd.addAll(types.stream().map(t -> URI.create(t.toString())).collect(Collectors.toList()));
        return toAdd;
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createClassAssertion(attribute.isInferred());
    }
}
