/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.exceptions;

/**
 * This exception is thrown when cardinality restriction constraint is violated.
 * <p>
 * For example when the model expects only single value of a property but multiple are present.
 */
public class CardinalityConstraintViolatedException extends IntegrityConstraintViolatedException {

    public CardinalityConstraintViolatedException(String message) {
        super(message);
    }
}
