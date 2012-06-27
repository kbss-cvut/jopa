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
import cz.cvut.kbss.jopa.model.annotations.SequenceType;

/**
 * Instances of the type ListAttribute represent persistent
 * java.util.List-valued attributes.
 * 
 * @param <X>
 *            The type the represented List belongs to
 * @param <E>
 *            The element type of the represented List
 */
public interface ListAttribute<X, E> extends
		PluralAttribute<X, java.util.List<E>, E> {

	@NonJPA
	public SequenceType getSequenceType();

	@NonJPA
	public IRI getOWLListClass();

	@NonJPA
	public IRI getOWLPropertyHasContentsIRI();

	@NonJPA
	public IRI getOWLObjectPropertyHasNextIRI();
}
