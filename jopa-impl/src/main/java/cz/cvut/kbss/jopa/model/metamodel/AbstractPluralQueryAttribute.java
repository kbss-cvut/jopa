package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Map;

public abstract class AbstractPluralQueryAttribute<X, C, E> extends AbstractQueryAttribute<X, C>
        implements PluralQueryAttribute<X, C, E> {

    private final Type<E> elementType;

    private final Class<C> collectionType;

    public AbstractPluralQueryAttribute(String query, Field field, ManagedType<X> declaringType,
                                        ParticipationConstraint[] constraints, Type<E> elementType,
                                        Class<C> collectionType, ConverterWrapper converter) {
        super(query, field, declaringType, constraints, converter);
        this.elementType = elementType;
        this.collectionType = collectionType;
    }

    @Override
    public boolean isCollection() {
        return true;
    }

    @Override
    public Class<E> getBindableJavaType() {
        return elementType.getJavaType();
    }

    @Override
    public cz.cvut.kbss.jopa.model.metamodel.Bindable.BindableType getBindableType() {
        return BindableType.PLURAL_ATTRIBUTE;
    }

    @Override
    public cz.cvut.kbss.jopa.model.metamodel.CollectionType getCollectionType() {
        if (getJavaType().isAssignableFrom(Collection.class)) {
            return CollectionType.COLLECTION;
        } else if (getJavaType().isAssignableFrom(Map.class)) {
            return CollectionType.MAP;
        } else {
            throw new IllegalArgumentException();
        }
    }

    @Override
    public Type<E> getElementType() {
        return elementType;
    }

    @Override
    public Class<C> getJavaType() {
        return collectionType;
    }
}
