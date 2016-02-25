/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.model.metamodel.SetAttribute;

import java.util.Set;

public class SetAttributeImpl<X, V> extends PluralAttributeImpl<X, Set<V>, V>
		implements SetAttribute<X, V> {

	private SetAttributeImpl(SetAttributeBuilder<X, V> builder) {
		super(builder);
	}

	@Override
	public CollectionType getCollectionType() {
		return CollectionType.SET;
	}

	@Override
	public String toString() {
		return "SetAttribute[" + getName() + "]";
	}

	public static PluralAttributeBuilder iri(IRI iri) {
		return new SetAttributeBuilder().collectionType(Set.class).iri(iri);
	}

	public static class SetAttributeBuilder<X, V> extends PluralAttributeBuilder<X, Set<V>, V> {

		public SetAttributeImpl<X, V> build() {
			return new SetAttributeImpl<>(this);
		}
	}

}
