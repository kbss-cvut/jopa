package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;

import java.lang.reflect.Field;
import java.lang.reflect.Member;

public abstract class AbstractQueryAttribute<X, Y> implements QueryAttribute<X, Y> {

    private final String query;

    private final Field field;

    private final ManagedType<X> declaringType;

    private final ParticipationConstraint[] constraints;

    private final ConverterWrapper converter;

    public AbstractQueryAttribute(String query, Field field, ManagedType<X> declaringType,
                                  ParticipationConstraint[] constraints, ConverterWrapper converter) {
        this.query = query;
        this.field = field;
        this.declaringType = declaringType;
        this.constraints = constraints;
        this.converter = converter;
    }

    @Override
    public String getQuery() {
        return query;
    }

    @Override
    public Member getJavaMember() {
        return field;
    }

    @Override
    public ParticipationConstraint[] getConstraints() {
        return constraints;
    }

    @Override
    public ManagedType<X> getDeclaringType() {
        return declaringType;
    }

    @Override
    public Field getJavaField() {
        return field;
    }

    @Override
    public FetchType getFetchType() {
        return null;
    }

    /**
     * A query based attribute is always inferred.
     *
     * @return always {@code true}
     */
    @Override
    public boolean isInferred() {
        return true;
    }

    /**
     * A query based attribute always includes explicit.
     *
     * @return always {@code true}
     */
    @Override
    public boolean includeExplicit() {
        return true;
    }

    @Override
    public String getName() {
        return null;
    }

    @Override
    public boolean isCollection() {
        return false;
    }

    public ConverterWrapper getConverter() {
        return converter;
    }

    @Override
    public String toString() {
        return declaringType.getJavaType().getSimpleName() + "." + getName();
    }
}
