package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.jopa.utils.CollectionFactory;

import java.lang.reflect.Field;

public class PluralQueryAttributeImpl<X, C, E> extends AbstractQueryAttribute<X, C>
        implements PluralQueryAttribute<X, C, E> {

    private final Type<E> elementType;

    private final Class<C> collectionType;

    public PluralQueryAttributeImpl(String query, Field field, ManagedType<X> declaringType, FetchType fetchType,
                                    ParticipationConstraint[] constraints, Type<E> elementType,
                                    Class<C> collectionType, ConverterWrapper converter) {
        super(query, field, declaringType, fetchType, constraints, converter);
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
        return CollectionFactory.resolveCollectionType(getJavaType());
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
