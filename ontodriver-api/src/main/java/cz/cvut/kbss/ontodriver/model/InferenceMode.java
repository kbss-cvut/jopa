/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
package cz.cvut.kbss.ontodriver.model;

/**
 * Indicates whether to consider inferred data and under which conditions.
 */
public enum InferenceMode {

    /**
     * Consider only explicit (asserted) data.
     */
    EXPLICIT,
    /**
     * Consider only inferred data.
     */
    INFERRED,
    /**
     * Consider both explicit (asserted) and inferred data.
     */
    EXPLICIT_AND_INFERRED
}
