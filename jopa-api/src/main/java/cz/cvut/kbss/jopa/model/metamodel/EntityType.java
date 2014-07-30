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

package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.NonJPA;
import cz.cvut.kbss.jopa.model.IRI;

/**
 * Instances of the type EntityType represent entity types.
 * 
 * @param <X>
 *            The represented entity type.
 */
public interface EntityType<X> extends IdentifiableType<X>, Bindable<X> {
	/**
	 * Return the entity name.
	 * 
	 * @return entity name
	 */
	String getName();

	@NonJPA
	IRI getIRI();

	@NonJPA
	Identifier getIdentifier();

	@NonJPA
	TypesSpecification<? super X, ?> getTypes();

	@NonJPA
	PropertiesSpecification<? super X, ?> getProperties();

	@NonJPA
	FieldSpecification<? super X, ?> getFieldSpecification(String fieldName);
}
