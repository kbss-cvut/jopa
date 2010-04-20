package cz.cvut.kbss.owlpersistence.owlapi;

import java.lang.reflect.Field;
import java.util.Set;

import cz.cvut.kbss.owlpersistence.model.IRI;
import cz.cvut.kbss.owlpersistence.model.annotations.CascadeType;
import cz.cvut.kbss.owlpersistence.model.annotations.FetchType;
import cz.cvut.kbss.owlpersistence.model.metamodel.ManagedType;
import cz.cvut.kbss.owlpersistence.model.metamodel.SetAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.Type;

public class SetAttributeImpl<X, V> extends PluralAttributeImpl<X, Set<V>, V>
		implements SetAttribute<X, V> {

	SetAttributeImpl(ManagedType<X> declaringType, String name, IRI iri,
			Class<Set<V>> collectionType, Type<V> elementType, Field member,
			PersistentAttributeType pat, final CascadeType[] cascadeTypes,
			final FetchType fetchType) {
		super(declaringType, name, iri, collectionType, elementType, member,
				pat, cascadeTypes, fetchType);
	}

	@Override
	public CollectionType getCollectionType() {
		return CollectionType.SET;
	}

	@Override
	public String toString() {
		return "SetAttribute[" + getName() + "]";
	}

}
