/**
 * Copyright (C) 2016 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.util.Collection;

abstract class PluralObjectPropertyStrategy<X> extends FieldStrategy<Attribute<? super X, ?>, X> {

    final PluralAttribute<? super X, ?, ?> pluralAtt;
    private Collection<Object> values;

    PluralObjectPropertyStrategy(EntityType<X> et, Attribute<? super X, ?> att, Descriptor descriptor,
                                 EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
        this.pluralAtt = (PluralAttribute<? super X, ?, ?>) attribute;
        this.values = CollectionFactory.createDefaultCollection(pluralAtt.getCollectionType());
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        final NamedResource valueIdentifier = (NamedResource) ax.getValue().getValue();
        if (IdentifierTransformer.isValidIdentifierType(pluralAtt.getBindableJavaType())) {
            values.add(IdentifierTransformer
                    .transformToIdentifier(valueIdentifier.getIdentifier(), pluralAtt.getBindableJavaType()));
        } else {
            final Object value = mapper.getEntityFromCacheOrOntology(pluralAtt.getBindableJavaType(),
                    valueIdentifier.getIdentifier(), attributeDescriptor);
            values.add(value);
        }

    }

    @Override
    void buildInstanceFieldValue(Object instance) throws IllegalAccessException {
        if (!values.isEmpty()) {
            setValueOnInstance(instance, values);
        }
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createObjectPropertyAssertion(pluralAtt.getIRI().toURI(),
                attribute.isInferred());
    }
}
