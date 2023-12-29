/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.model.query.criteria;

import cz.cvut.kbss.jopa.model.query.TupleElement;

import java.util.List;

public interface Selection<X> extends TupleElement<X> {

    /**
     * Whether the selection item is a compound selection.
     * Compounded selection is NOT YET SUPPORTED. Therefore, the method always returns false.
     * @return boolean indicating whether the selection is a compound selection
     */
    boolean isCompoundedSelection();

    /**
     * Return the selection items composing a compound selection. Modifications to the list do not affect the query.
     * Compounded selection is NOT YET SUPPORTED. Therefore, the method always throws IllegalStateException.
     * @return list of selection items
     * @throws IllegalStateException if selection is not a compound selection
     */
    List<Selection<?>> getCompoundedSelectionItems();
}
