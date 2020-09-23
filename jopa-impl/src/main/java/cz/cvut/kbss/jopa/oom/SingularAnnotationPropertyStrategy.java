/**
 * Copyright (C) 2020 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.oom.converter.ToLexicalFormConverter;
import cz.cvut.kbss.jopa.utils.IdentifierTransformer;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

class SingularAnnotationPropertyStrategy<X> extends SingularDataPropertyStrategy<X> {

    SingularAnnotationPropertyStrategy(EntityType<X> et, AbstractAttribute<? super X, ?> att, Descriptor descriptor,
                                       EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        final Object val = ax.getValue().getValue();
        if (!isValidRange(val)) {
            return;
        }
        verifyCardinalityConstraint(ax.getSubject());
        if (IdentifierTransformer.isValidIdentifierType(attribute.getJavaType())) {
            this.value = IdentifierTransformer
                    .transformToIdentifier(ToLexicalFormConverter.INSTANCE.convertToAttribute(val),
                            attribute.getJavaType());
        } else {
            this.value = toAttributeValue(val);
        }
    }

    @Override
    boolean isValidRange(Object value) {
        assert value != null;

        return value instanceof NamedResource && IdentifierTransformer.isValidIdentifierType(attribute.getJavaType()) ||
                super.isValidRange(value);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final Object value = extractFieldValueFromInstance(instance);
        if (value == null) {
            valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeContext());
            return;
        }

        if (IdentifierTransformer.isValidIdentifierType(attribute.getJavaType()) &&
                !attribute.getJavaType().isAssignableFrom(String.class)) {
            valueBuilder.addValue(createAssertion(),
                    new Value<>(NamedResource.create(IdentifierTransformer.valueAsUri(value))), getAttributeContext());
        } else if (value instanceof MultilingualString) {
            valueBuilder.addValues(createAssertion(),
                    SingularMultilingualStringFieldStrategy.translationsToLangStrings((MultilingualString) value),
                    getAttributeContext());
        } else {
            valueBuilder.addValue(createAssertion(), new Value<>(toAxiomValue(value)), getAttributeContext());
        }
    }

    @Override
    Assertion createAssertion() {
        return Assertion
                .createAnnotationPropertyAssertion(attribute.getIRI().toURI(), getLanguage(), attribute.isInferred());
    }
}
