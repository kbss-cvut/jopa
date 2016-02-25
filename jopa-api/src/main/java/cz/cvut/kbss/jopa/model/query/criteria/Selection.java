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
package cz.cvut.kbss.jopa.model.query.criteria;

import java.util.List;

import cz.cvut.kbss.jopa.model.query.TupleElement;

/**
 * The Selection interface defines an item that is to be returned in a query
 * result.
 * 
 * @param <X>
 *            the type of the selection item
 */
public interface Selection<X> extends TupleElement<X> {
	/**
	 * Assigns an alias to the selection item. Once assigned, an alias cannot be
	 * changed or reassigned. Returns the same selection item.
	 * 
	 * @param name
	 *            alias
	 * @return selection item
	 */
	Selection<X> alias(String name);

	/**
	 * Whether the selection item is a compound selection.
	 * 
	 * @return boolean indicating whether the selection is a
	 * 
	 *         compound selection
	 */
	boolean isCompoundSelection();

	/**
	 * Return the selection items composing a compound selection. Modifications
	 * to the list do not affect the query.
	 * 
	 * @return list of selection items
	 * @throws IllegalStateException
	 *             if selection is not a compound
	 * 
	 *             selection
	 */
	List<Selection<?>> getCompoundSelectionItems();
}
