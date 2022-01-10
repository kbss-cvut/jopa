/**
 * Copyright (C) 2022 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.util.List;
import java.util.stream.Collectors;

class SingularMultilingualStringFieldStrategy<X>
        extends DataPropertyFieldStrategy<AbstractAttribute<? super X, MultilingualString>, X> {

    private MultilingualString value;

    SingularMultilingualStringFieldStrategy(EntityType<X> et, AbstractAttribute<? super X, MultilingualString> att,
                                            Descriptor entityDescriptor, EntityMappingHelper mapper) {
        super(et, att, entityDescriptor, mapper);
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        if (value == null) {
            this.value = new MultilingualString();
        }
        final Value<?> axiomValue = ax.getValue();
        if (axiomValue.getValue() instanceof LangString) {
            final LangString lsAxiomValue = (LangString) axiomValue.getValue();
            verifyCardinality(lsAxiomValue.getLanguage().orElse(null), ax.getSubject());
            value.set(lsAxiomValue.getLanguage().orElse(null), lsAxiomValue.getValue());
        } else {
            value.set(axiomValue.stringValue());
        }
    }

    private void verifyCardinality(String language, NamedResource subject) {
        if (language != null && value.contains(language)) {
            throw new CardinalityConstraintViolatedException(String.format(
                    "Expected single value with language tag %s of attribute %s of instance <%s>, but got multiple.",
                    language, attribute.getName(), subject));
        } else if (language == null && value.containsSimple()) {
            throw new CardinalityConstraintViolatedException(String.format(
                    "Expected single simple literal value of attribute %s of instance <%s>, but got multiple.",
                    attribute.getName(), subject));
        }
    }

    @Override
    void buildInstanceFieldValue(Object instance) {
        setValueOnInstance(instance, value);
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final MultilingualString attValue = (MultilingualString) extractFieldValueFromInstance(instance);
        if (attValue == null || attValue.isEmpty()) {
            valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeWriteContext());
        } else {
            valueBuilder.addValues(createAssertion(), translationsToLangStrings(attValue), getAttributeWriteContext());
        }
    }

    static List<Value<?>> translationsToLangStrings(MultilingualString str) {
        return str.getValue().entrySet().stream().map(e -> new Value<>(new LangString(e.getValue(), e.getKey())))
                  .collect(Collectors.toList());
    }
}
