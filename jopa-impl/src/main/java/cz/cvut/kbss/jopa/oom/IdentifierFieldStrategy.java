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
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.model.*;

import java.util.Collection;
import java.util.Collections;
import java.util.Set;

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
    void buildInstanceFieldValue(Object instance) {
        // Do nothing
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        valueBuilder.addValue(createAssertion(), new Value<>(et.getIRI().toURI()), getAttributeWriteContext());
    }

    @Override
    Set<Axiom<?>> buildAxiomsFromInstance(X instance) {
        return Collections.singleton(
                new AxiomImpl<>(NamedResource.create(EntityPropertiesUtils.getIdentifier(instance, et)),
                                createAssertion(),
                                new Value<>(et.getIRI().toURI())));
    }

    @Override
    Collection<Value<?>> toAxiomValue(Object value) {
        return Collections.singleton(new Value<>(et.getIRI().toURI()));
    }

    @Override
    Assertion createAssertion() {
        assert !attribute.isInferred();
        return Assertion.createClassAssertion(attribute.isInferred());
    }
}
