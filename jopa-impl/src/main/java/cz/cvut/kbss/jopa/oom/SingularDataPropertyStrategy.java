package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.Value;

class SingularDataPropertyStrategy<X> extends FieldStrategy<Attribute<? super X, ?>, X> {

    private Object value;

    SingularDataPropertyStrategy(EntityType<X> et, Attribute<? super X, ?> att,
                                 Descriptor descriptor, EntityMappingHelper mapper) {
        super(et, att, descriptor, mapper);
    }

    @Override
    void addValueFromAxiom(Axiom<?> ax) {
        final Value<?> val = ax.getValue();
        if (!isValidRange(val.getValue())) {
            return;
        }
        if (value != null) {
            throw new CardinalityConstraintViolatedException(
                    "Expected single value of attribute " + attribute.getName() + ", but got multiple");
        }
        this.value = val.getValue();
    }

    private boolean isValidRange(Object value) {
        return attribute.getJavaField().getType().isAssignableFrom(value.getClass()) || isFieldEnum();
    }

    private boolean isFieldEnum() {
        final Class<?> cls = attribute.getJavaField().getType();
        return cls.isEnum();
    }

    @Override
    void buildInstanceFieldValue(Object entity) throws IllegalAccessException {
        final Object toAssign = isFieldEnum() ? resolveEnumValue() : value;
        setValueOnInstance(entity, toAssign);
    }

    private Object resolveEnumValue() {
        final Class cls = attribute.getJavaField().getType();
        return Enum.valueOf(cls, value.toString());
    }

    @Override
    void buildAxiomValuesFromInstance(X instance, AxiomValueGatherer valueBuilder) throws IllegalAccessException {
        final Object extractedValue = extractFieldValueFromInstance(instance);

        final Value<?> val = extractedValue != null ? new Value<>(extractedValue) : Value.nullValue();
        valueBuilder.addValue(createAssertion(), val, getAttributeContext());
    }

    @Override
    Assertion createAssertion() {
        return Assertion.createDataPropertyAssertion(attribute.getIRI().toURI(),
                attribute.isInferred());
    }
}
