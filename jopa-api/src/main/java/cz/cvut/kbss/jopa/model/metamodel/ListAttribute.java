/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
public interface ListAttribute<X, E> extends PluralAttribute<X, java.util.List<E>, E> {

    /**
     * Gets the type of the sequence.
     * @return List type
     */
    @NonJPA
    SequenceType getSequenceType();

    /**
     * Gets the IRI of the class that represents the 'OWLList' concept.
     *
     * This is relevant only for referenced lists.
     * @return List type IRI
     */
    @NonJPA
    IRI getOWLListClass();

    /**
     * Gets IRI of the property representing the relation between a list node and its content (value).
     *
     * Relevant only for referenced lists.
     * @return Property IRI
     */
    @NonJPA
    IRI getOWLPropertyHasContentsIRI();

    /**
     * Gets IRI of the property representing next node in the list.
     * @return Property IRI
     */
    @NonJPA
    IRI getOWLObjectPropertyHasNextIRI();
}
