/**
 * Copyright (C) 2019 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractPluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;

abstract class PluralObjectPropertyStrategy<Y extends AbstractPluralAttribute<? super X, ?, ?>, X>
        extends FieldStrategy<Y, X> {

    private static final Logger LOG = LoggerFactory.getLogger(PluralObjectPropertyStrategy.class);

    private Collection<Object> values;

    PluralObjectPropertyStrategy(EntityType<X> et, Y att, Descriptor descriptor,
                                 EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
        this.values = CollectionFactory.createDefaultCollection(att.getCollectionType());
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        final NamedResource valueIdentifier = (NamedResource) ax.getValue().getValue();
        if (IdentifierTransformer.isValidIdentifierType(attribute.getBindableJavaType())) {
            values.add(IdentifierTransformer
                    .transformToIdentifier(valueIdentifier.getIdentifier(), attribute.getBindableJavaType()));
        } else {
            final Object value = mapper.getEntityFromCacheOrOntology(attribute.getBindableJavaType(),
                    valueIdentifier.getIdentifier(), entityDescriptor.getAttributeDescriptor(attribute));
            if (value != null) {
                values.add(value);
            } else {
                LOG.trace("Value of axiom {} could not be loaded as entity filling attribute {}.", ax, attribute);
            }
        }

    }

    @Override
    void buildInstanceFieldValue(Object instance) {
        if (!values.isEmpty()) {
            setValueOnInstance(instance, values);
        }
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createObjectPropertyAssertion(attribute.getIRI().toURI(), attribute.isInferred());
    }
}
