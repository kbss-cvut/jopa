/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractPluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;
import cz.cvut.kbss.ontodriver.model.*;

import java.util.Collection;
import java.util.Collections;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

public class PluralMultilingualStringFieldStrategy<X>
        extends DataPropertyFieldStrategy<AbstractPluralAttribute<? super X, ?, MultilingualString>, X> {

    private final Collection<MultilingualString> values;

    PluralMultilingualStringFieldStrategy(EntityType<X> et,
                                          AbstractPluralAttribute<? super X, ?, MultilingualString> att,
                                          Descriptor entityDescriptor,
                                          EntityMappingHelper mapper) {
        super(et, att, entityDescriptor, mapper);
        this.values = CollectionFactory.createDefaultCollection(att.getCollectionType());
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        String value;
        String language = null;
        if (ax.getValue().getValue() instanceof LangString) {
            final LangString ls = (LangString) ax.getValue().getValue();
            value = ls.getValue();
            language = ls.getLanguage().orElse(null);
        } else {
            value = ax.getValue().stringValue();
        }
        addValue(value, language);
    }

    private void addValue(String value, String language) {
        for (MultilingualString ms : values) {
            if (!ms.contains(language)) {
                ms.set(language, value);
                return;
            }
        }
        final MultilingualString newOne = new MultilingualString();
        newOne.set(language, value);
        values.add(newOne);
    }

    @Override
    void buildInstanceFieldValue(Object instance) {
        if (!values.isEmpty()) {
            setValueOnInstance(instance, values);
        }
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) {
        final Object value = extractFieldValueFromInstance(instance);
        assert value instanceof Collection || value == null;
        final Collection<MultilingualString> valueCollection = (Collection<MultilingualString>) value;
        if (valueCollection == null || valueCollection.isEmpty()) {
            valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeWriteContext());
        } else {
            final Set<Value<?>> assertionValues = translationsToValues(valueCollection);
            valueBuilder.addValues(createAssertion(),
                    filterOutInferredValues(valueBuilder.getSubjectIdentifier(), assertionValues),
                    getAttributeWriteContext());
        }
    }

    private static Set<Value<?>> translationsToValues(Collection<MultilingualString> valueCollection) {
        return valueCollection.stream()
                              .filter(Objects::nonNull)
                              .flatMap(SingularMultilingualStringFieldStrategy::translationsToLangStrings)
                              .collect(Collectors.toSet());
    }

    @Override
    Set<Axiom<?>> buildAxiomsFromInstance(X instance) {
        final Object value = extractFieldValueFromInstance(instance);
        assert value instanceof Collection || value == null;
        final Collection<MultilingualString> valueCollection = (Collection<MultilingualString>) value;
        if (valueCollection == null || valueCollection.isEmpty()) {
            return Collections.emptySet();
        }
        final Set<Value<?>> assertionValues = translationsToValues(valueCollection);
        final NamedResource subject = NamedResource.create(EntityPropertiesUtils.getIdentifier(instance, et));
        final Assertion assertion = createAssertion();
        return assertionValues.stream().map(v -> new AxiomImpl<>(subject, assertion, v)).collect(Collectors.toSet());
    }

    @Override
    Collection<Value<?>> toAxiomValue(Object value) {
        assert value instanceof MultilingualString;
        return SingularMultilingualStringFieldStrategy.translationsToLangStrings((MultilingualString) value)
                                                      .collect(Collectors.toList());
    }
}
