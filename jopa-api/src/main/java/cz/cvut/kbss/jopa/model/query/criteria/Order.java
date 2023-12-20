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

public interface Order {
    /**
     * Return the expression that is used for ordering.
     *
     * @return expression used for ordering
     */
    Expression<?> getExpression();

    /**
     * Whether ascending ordering is in effect.
     *
     * @return boolean indicating whether ordering is ascending
     */
    boolean isAscending();

    /**
     * Switch the ordering.
     *
     * @return a new Order instance with the reversed ordering
     */
    Order reverse();
}
