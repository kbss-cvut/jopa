package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;

import java.lang.reflect.Field;

/**
 * Singular query attributes contain a single value or reference, ie. they are not collections.
 *
 * @param <X> The represented type that contains the attribute
 * @param <Y> The type of the represented attribute
 */
public class SingularQueryAttributeImpl<X, Y> extends AbstractQueryAttribute<X, Y> implements SingularQueryAttribute<X, Y> {

    private final Type<Y> type;

    public SingularQueryAttributeImpl(String query, Field field, ManagedType<X> declaringType, FetchType fetchType,
                                      Type<Y> type, ParticipationConstraint[] constraints, ConverterWrapper converter) {
        super(query, field, declaringType, fetchType, constraints, converter);
        this.type = type;
    }

    @Override
    public Type<Y> getType() {
        return type;
    }

    @Override
    public Class<Y> getJavaType() {
        return type.getJavaType();
    }
}
