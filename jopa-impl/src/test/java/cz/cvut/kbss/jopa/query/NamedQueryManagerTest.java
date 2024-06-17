/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class NamedQueryManagerTest {

    private static final String QUERY = "SELECT ?x ?y ?z WHERE { ?x ?y ?z .}";

    private NamedQueryManager queryManager;

    @BeforeEach
    public void setUp() {
        this.queryManager = new NamedQueryManager();
    }

    @Test
    public void addedQueryCanBeRetrieved() {
        final String name = "selectAll";
        queryManager.addNamedQuery(name, QUERY);
        final String res = queryManager.getQuery(name);
        assertEquals(QUERY, res);
    }

    @Test
    public void addingQueryWithExistingNameThrowsIllegalArgument() {
        final String name = "selectAll";
        queryManager.addNamedQuery(name, QUERY);
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> queryManager.addNamedQuery(name, "TEST"));
        assertEquals("Query with name " + name + " already exists in this persistence unit.", ex.getMessage());
    }

    @Test
    public void retrievingUnknownQueryThrowsIllegalArgument() {
        final String name = "selectAll";
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> queryManager.getQuery(name));
        assertEquals("Query with name " + name + " was not found in this persistence unit.", ex.getMessage());
    }
}
