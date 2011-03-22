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
import java.util.Set;

import cz.cvut.kbss.owlpersistence.model.IRI;
import cz.cvut.kbss.owlpersistence.model.annotations.CascadeType;
import cz.cvut.kbss.owlpersistence.model.annotations.FetchType;
import cz.cvut.kbss.owlpersistence.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.owlpersistence.model.metamodel.ManagedType;
import cz.cvut.kbss.owlpersistence.model.metamodel.SetAttribute;
import cz.cvut.kbss.owlpersistence.model.metamodel.Type;

public class SetAttributeImpl<X, V> extends PluralAttributeImpl<X, Set<V>, V>
		implements SetAttribute<X, V> {

	SetAttributeImpl(ManagedType<X> declaringType, String name, IRI iri,
			Class<Set<V>> collectionType, Type<V> elementType, Field member,
			PersistentAttributeType pat, final CascadeType[] cascadeTypes,
			final FetchType fetchType, boolean inferred,
			ParticipationConstraint[] constraints) {
		super(declaringType, name, iri, collectionType, elementType, member,
				pat, cascadeTypes, fetchType, inferred, constraints);
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
