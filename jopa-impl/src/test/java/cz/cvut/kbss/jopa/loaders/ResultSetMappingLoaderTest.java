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
package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMappings;
import cz.cvut.kbss.jopa.model.annotations.VariableResult;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class ResultSetMappingLoaderTest {

    private final ResultSetMappingLoader sut = new ResultSetMappingLoader();

    @Test
    public void doesNothingWhenClassWithoutResultSetMappingIsPassedIn() {
        sut.accept(ResultSetMappingLoaderTest.class);
        assertTrue(sut.getMappings().isEmpty());
    }

    @Test
    public void registersSingleResultSetMappingDeclaredOnClass() {
        sut.accept(ClassWithSingleMapping.class);
        assertEquals(1, sut.getMappings().size());
        assertEquals(ClassWithSingleMapping.class.getDeclaredAnnotation(SparqlResultSetMapping.class),
                sut.getMappings().iterator().next());
    }

    @SparqlResultSetMapping(name = "testMapping", variables = {
            @VariableResult(name = "x"),
            @VariableResult(name = "y")
    })
    private static class ClassWithSingleMapping {
    }

    @Test
    public void registersAllItemsFromResultSetMappingsDeclaredOnClass() {
        sut.accept(ClassWithMappings.class);
        assertEquals(2, sut.getMappings().size());
        final SparqlResultSetMappings mappings = ClassWithMappings.class
                .getDeclaredAnnotation(SparqlResultSetMappings.class);
        for (SparqlResultSetMapping m : mappings.value()) {
            assertTrue(sut.getMappings().contains(m));
        }
    }

    @SparqlResultSetMappings({
            @SparqlResultSetMapping(name = "testMappingTwo", variables = {
                    @VariableResult(name = "x"),
                    @VariableResult(name = "y")
            }),
            @SparqlResultSetMapping(name = "testMappingTwo", variables = {
                    @VariableResult(name = "z")
            })
    })
    private static class ClassWithMappings {
    }

    @Test
    public void registersSingleItemFromResultSetMappingsDeclaredOnClass() {
        sut.accept(ClassWithMappingInMappings.class);
        assertEquals(1, sut.getMappings().size());
        final SparqlResultSetMappings mappings = ClassWithMappingInMappings.class
                .getDeclaredAnnotation(SparqlResultSetMappings.class);
        for (SparqlResultSetMapping m : mappings.value()) {
            assertTrue(sut.getMappings().contains(m));
        }
    }

    @SparqlResultSetMappings({
            @SparqlResultSetMapping(name = "testMapping", variables = {
                    @VariableResult(name = "x"),
                    @VariableResult(name = "y")
            })
    })
    private static class ClassWithMappingInMappings {
    }
}
