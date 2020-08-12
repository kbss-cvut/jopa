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
            value.set(lsAxiomValue.getValue(), lsAxiomValue.getLanguage().orElse(null));
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
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder)
            throws IllegalAccessException {

    }
}
