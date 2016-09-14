package cz.cvut.kbss.jopa.model.metamodel;

public class MappedSuperclassTypeImpl<X> extends AbstractIdentifiableType<X> implements MappedSuperclassType<X> {

    MappedSuperclassTypeImpl(Class<X> javaType) {
        super(javaType);
    }

    @Override
    public PersistenceType getPersistenceType() {
        return PersistenceType.MAPPED_SUPERCLASS;
    }
}
