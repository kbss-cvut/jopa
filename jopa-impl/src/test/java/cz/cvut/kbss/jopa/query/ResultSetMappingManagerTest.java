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
package cz.cvut.kbss.jopa.query;

import cz.cvut.kbss.jopa.query.mapper.ResultRowMapper;
import cz.cvut.kbss.jopa.query.mapper.SparqlResultMapper;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class ResultSetMappingManagerTest {

    private static final String NAME = "testMapping";

    private final ResultSetMappingManager manager = new ResultSetMappingManager();

    @Test
    public void addedMapperCanBeRetrieved() {
        final SparqlResultMapper mapper = new ResultRowMapper(NAME);
        manager.addMapper(NAME, mapper);
        final SparqlResultMapper result = manager.getMapper(NAME);
        assertNotNull(result);
        assertEquals(mapper, result);
    }

    @Test
    public void addMapperThrowsIllegalArgumentWhenMapperForSpecifiedNameAlreadyExists() {
        final SparqlResultMapper mapper = new ResultRowMapper(NAME);
        manager.addMapper(NAME, mapper);
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> manager.addMapper(NAME, new ResultRowMapper(NAME)));
        assertEquals("Mapping " + NAME + " already exists in this persistence unit.", ex.getMessage());
    }

    @Test
    public void getMapperThrowsIllegalArgumentWhenMapperForMappingNameDoesNotExist() {
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> manager.getMapper(NAME));
        assertEquals("Mapping " + NAME + " not found in this persistence unit.", ex.getMessage());
    }
}
