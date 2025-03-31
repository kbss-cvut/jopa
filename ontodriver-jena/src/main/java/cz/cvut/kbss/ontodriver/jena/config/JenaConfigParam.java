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
package cz.cvut.kbss.ontodriver.jena.config;

import cz.cvut.kbss.ontodriver.config.ConfigurationParameter;

public enum JenaConfigParam implements ConfigurationParameter {

    ISOLATION_STRATEGY(JenaOntoDriverProperties.JENA_ISOLATION_STRATEGY),
    STORAGE_TYPE(JenaOntoDriverProperties.JENA_STORAGE_TYPE),
    TREAT_DEFAULT_GRAPH_AS_UNION(JenaOntoDriverProperties.JENA_TREAT_DEFAULT_GRAPH_AS_UNION);

    private final String name;

    JenaConfigParam(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return name;
    }
}
