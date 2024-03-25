/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.*;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

class TypesFieldStrategy<X> extends FieldStrategy<TypesSpecification<? super X, ?>, X> {

    private final Set<Object> values = new HashSet<>();

    TypesFieldStrategy(EntityType<X> et, TypesSpecification<? super X, ?> att, Descriptor descriptor,
                       EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        if (MappingUtils.isEntityClassAssertion(ax, et)) {
            return;
        }
        final Object type =
                IdentifierTransformer.transformToIdentifier(ax.getValue().getValue(), attribute.getElementType());
        values.add(type);
    }

    @Override
    void buildInstanceFieldValue(Object instance) {
        assert attribute.getJavaField().getType().isAssignableFrom(Set.class);
        setValueOnInstance(instance, !values.isEmpty() ? values : null);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
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
            valueBuilder.removeTypes(prepareTypes(origTypes), getAttributeWriteContext());
        } else {
            assert val instanceof Set;  // This is verified when the metamodel is built
            final Set<?> types = (Set<?>) val;
            if (original == null) {
                valueBuilder.addTypes(prepareTypes(types), getAttributeWriteContext());
            } else {
                Set<?> origTypes = (Set<?>) extractFieldValueFromInstance(original);
                if (origTypes == null) {
                    origTypes = Collections.emptySet();
                }
                extractTypesToAdd(valueBuilder, types, origTypes);
                extractTypesToRemove(valueBuilder, types, origTypes);
            }
        }
    }

    private void extractTypesToAdd(AxiomValueGatherer valueBuilder, Set<?> types, Set<?> origTypes) {
        final Set<URI> toAdd = typesDiff(origTypes, types);
        valueBuilder.addTypes(toAdd, getAttributeWriteContext());
    }

    private static Set<URI> typesDiff(Set<?> base, Set<?> difference) {
        final Set<URI> addedDiff = new HashSet<>(base.size());
        addedDiff.addAll(difference.stream().filter(t -> !base.contains(t)).map(t -> URI.create(t.toString()))
                                   .collect(Collectors.toList()));
        return addedDiff;
    }

    private void extractTypesToRemove(AxiomValueGatherer valueBuilder, Set<?> types, Set<?> origTypes) {
        final Set<URI> toRemove = typesDiff(types, origTypes);
        valueBuilder.removeTypes(toRemove, getAttributeWriteContext());
    }

    private static Set<URI> prepareTypes(Set<?> types) {
        final Set<URI> toAdd = new HashSet<>(types.size());
        toAdd.addAll(types.stream().map(t -> URI.create(t.toString())).collect(Collectors.toList()));
        return toAdd;
    }

    @Override
    Set<Axiom<?>> buildAxiomsFromInstance(X instance) {
        final Object val = extractFieldValueFromInstance(instance);
        assert val == null || val instanceof Set;
        final Set<?> types = (Set<?>) val;
        if (val == null || ((Set<?>) val).isEmpty()) {
            return Collections.emptySet();
        } else {
            final NamedResource subject = NamedResource.create(EntityPropertiesUtils.getIdentifier(instance, et));
            final Assertion assertion = createAssertion();
            return types.stream().map(t -> new AxiomImpl<>(subject, assertion, new Value<>(URI.create(t.toString()))))
                        .collect(Collectors.toSet());
        }
    }

    @Override
    Collection<Value<?>> toAxiomValue(Object value) {
        return Collections.singleton(new Value<>(URI.create(value.toString())));
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createClassAssertion(attribute.isInferred());
    }
}
