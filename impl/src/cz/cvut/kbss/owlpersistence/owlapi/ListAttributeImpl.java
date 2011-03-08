package cz.cvut.kbss.owlpersistence.owlapi;

import java.lang.reflect.Field;
import java.util.List;

import cz.cvut.kbss.owlpersistence.model.IRI;
import cz.cvut.kbss.owlpersistence.model.annotations.CascadeType;
import cz.cvut.kbss.owlpersistence.model.annotations.FetchType;
import cz.cvut.kbss.owlpersistence.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.owlpersistence.model.annotations.SequenceType;
import cz.cvut.kbss.owlpersistence.model.metamodel.ListAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.ManagedType;
import cz.cvut.kbss.owlpersistence.model.metamodel.Type;

public class ListAttributeImpl<X, V> extends PluralAttributeImpl<X, List<V>, V>
		implements ListAttribute<X, V> {

	private final IRI owlListClass;

	private final IRI owlObjectPropertyHasNext;

	private final IRI owlPropertyHasContents;

	private final SequenceType owlSequenceType;

	ListAttributeImpl(ManagedType<X> declaringType, String name, IRI iri,
			Class<List<V>> collectionType, Type<V> elementType, Field member,
			PersistentAttributeType pat, CascadeType[] cascadetypes,
			final IRI owlListClass, final IRI owlObjectPropertyHasNext,
			final IRI owlPropertyHasContent, final SequenceType sequenceType,
			final FetchType fetchType, boolean inferred,
			ParticipationConstraint[] constraints) {
		super(declaringType, name, iri, collectionType, elementType, member,
				pat, cascadetypes, fetchType, inferred, constraints);

		this.owlListClass = owlListClass;
		this.owlObjectPropertyHasNext = owlObjectPropertyHasNext;
		this.owlPropertyHasContents = owlPropertyHasContent;
		this.owlSequenceType = sequenceType;
	}

	@Override
	public CollectionType getCollectionType() {
		return CollectionType.LIST;
	}

	@Override
	public IRI getOWLListClass() {
		return owlListClass;
	}

	@Override
	public IRI getOWLObjectPropertyHasNextIRI() {
		return owlObjectPropertyHasNext;
	}

	@Override
	public IRI getOWLPropertyHasContentsIRI() {
		return owlPropertyHasContents;
	}

	@Override
	public SequenceType getSequenceType() {
		return owlSequenceType;
	}

	@Override
	public String toString() {
		return "ListAttribute[" + getName() + "]";
	}

}
