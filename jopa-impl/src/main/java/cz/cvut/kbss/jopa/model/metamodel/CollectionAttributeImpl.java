package cz.cvut.kbss.jopa.model.metamodel;

import java.util.Collection;
import java.util.Set;

public class CollectionAttributeImpl<X, V> extends AbstractPluralAttribute<X, Collection<V>, V>
        implements CollectionAttribute<X, V> {

    private CollectionAttributeImpl(PluralAttributeBuilder<X, Collection<V>, V> builder) {
        super(builder);
    }

    @Override
    public CollectionType getCollectionType() {
        return CollectionType.COLLECTION;
    }

    @Override
    public String toString() {
        return "CollectionAttribute[" + getName() + "]";
    }

    public static PluralAttributeBuilder builder(PropertyAttributes config) {
        return new CollectionAttributeBuilder().collectionType(Set.class).config(config);
    }

    public static class CollectionAttributeBuilder<X, V> extends PluralAttributeBuilder<X, Collection<V>, V> {

        @Override
        public CollectionAttributeImpl<X, V> build() {
            return new CollectionAttributeImpl<>(this);
        }
    }
}
