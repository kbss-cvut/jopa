/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

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

	
	public CollectionType getCollectionType() {
		return CollectionType.LIST;
	}

	
	public IRI getOWLListClass() {
		return owlListClass;
	}

	
	public IRI getOWLObjectPropertyHasNextIRI() {
		return owlObjectPropertyHasNext;
	}

	
	public IRI getOWLPropertyHasContentsIRI() {
		return owlPropertyHasContents;
	}

	
	public SequenceType getSequenceType() {
		return owlSequenceType;
	}

	
	public String toString() {
		return "ListAttribute[" + getName() + "]";
	}

}
