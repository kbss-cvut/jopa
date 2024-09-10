package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.annotations.RDFContainerType;

public class RdfContainerAttributeImpl<X, C, E> extends AbstractPluralAttribute<X, C, E> implements RDFContainerAttribute<X, C, E> {

    private final RDFContainerType containerType;

    private final CollectionType collectionType;

    RdfContainerAttributeImpl(RDFContainerAttributeBuilder<X, C, E> builder) {
        super(builder);
        this.containerType = builder.containerType;
        this.collectionType = resolveCollectionType(containerType);
    }

    private static CollectionType resolveCollectionType(RDFContainerType containerType) {
        return switch (containerType) {
            case SEQ -> CollectionType.LIST;
            case ALT -> CollectionType.SET;
            case BAG -> CollectionType.COLLECTION;
        };
    }

    @Override
    public RDFContainerType getContainerType() {
        return containerType;
    }

    @Override
    public CollectionType getCollectionType() {
        return collectionType;
    }

    @Override
    public String toString() {
        return "RDFContainerAttribute[" + getName() + "]";
    }

    static RDFContainerAttributeBuilder builder(PropertyAttributes config) {
        return new RDFContainerAttributeBuilder().config(config);
    }

    static class RDFContainerAttributeBuilder<X, C, E> extends PluralAttributeBuilder<X, C, E> {

        private RDFContainerType containerType;

        public RDFContainerAttributeBuilder<X, C, E> containerType(RDFContainerType containerType) {
            this.containerType = containerType;
            return this;
        }

        @Override
        public RDFContainerAttributeBuilder<X, C, E> config(PropertyAttributes config) {
            super.config(config);
            return this;
        }

        @Override
        public RdfContainerAttributeImpl<X, C, E> build() {
            return new RdfContainerAttributeImpl<>(this);
        }
    }
}
