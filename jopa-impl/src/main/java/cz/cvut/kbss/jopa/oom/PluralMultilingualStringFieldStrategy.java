package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractPluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.utils.CollectionFactory;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.LangString;
import cz.cvut.kbss.ontodriver.model.Value;

import java.util.Collection;
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
                ms.set(value, language);
                return;
            }
        }
        final MultilingualString newOne = new MultilingualString();
        newOne.set(value, language);
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
            valueBuilder.addValue(createAssertion(), Value.nullValue(), getAttributeContext());
        } else {
            final Set<Value<?>> assertionValues = valueCollection.stream()
                                                                 .filter(Objects::nonNull)
                                                                 .flatMap(v -> v.getValue().entrySet().stream()
                                                                                .map(e -> new Value<>(
                                                                                        new LangString(e.getValue(),
                                                                                                e.getKey()))))
                                                                 .collect(Collectors.toSet());
            valueBuilder.addValues(createAssertion(), assertionValues, getAttributeContext());
        }
    }
}
