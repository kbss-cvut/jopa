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
package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.model.annotations.ConstructorResult;
import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.model.annotations.VariableResult;
import cz.cvut.kbss.jopa.query.ResultSetMappingManager;
import org.junit.Test;

import java.net.URI;
import java.util.List;

import static org.junit.Assert.*;

public class ResultSetMappingProcessorTest {

    private static final String MAPPING_NAME = "testMapping";

    private ResultSetMappingProcessor processor = new ResultSetMappingProcessor();

    @Test
    public void buildMapperCreatesRowMapperWithVariableMappersConfiguredInMappingAnnotation() throws Exception {
        processor.buildMapper(getMapping(WithVariableMapping.class));
        final ResultSetMappingManager manager = processor.getManager();
        final SparqlResultMapper mapper = manager.getMapper(MAPPING_NAME);
        assertNotNull(mapper);
        final ResultRowMapper rowMapper = (ResultRowMapper) mapper;
        assertEquals(MAPPING_NAME, rowMapper.getName());
        final List<SparqlResultMapper> rowMappers = rowMapper.getRowMappers();
        assertEquals(2, rowMappers.size());
        assertTrue(rowMappers.get(0) instanceof VariableResultMapper);
        assertEquals("x", ((VariableResultMapper) rowMappers.get(0)).getName());
        assertEquals(void.class, ((VariableResultMapper) rowMappers.get(0)).getTargetType());
        assertTrue(rowMappers.get(1) instanceof VariableResultMapper);
        assertEquals("y", ((VariableResultMapper) rowMappers.get(1)).getName());
        assertEquals(URI.class, ((VariableResultMapper) rowMappers.get(1)).getTargetType());
    }

    @SparqlResultSetMapping(name = MAPPING_NAME, variables = {
            @VariableResult(name = "x"),
            @VariableResult(name = "y", type = URI.class)
    })
    private static final class WithVariableMapping {
    }

    private static SparqlResultSetMapping getMapping(Class<?> cls) {
        return cls.getDeclaredAnnotation(SparqlResultSetMapping.class);
    }

    @Test
    public void buildMapperBuildsEmptyMapperFromEmptyMappingConfiguration() {
        processor.buildMapper(getMapping(EmptyMapping.class));
        final ResultSetMappingManager manager = processor.getManager();
        final SparqlResultMapper mapper = manager.getMapper(MAPPING_NAME);
        assertNotNull(mapper);
        final ResultRowMapper rowMapper = (ResultRowMapper) mapper;
        assertEquals(MAPPING_NAME, rowMapper.getName());
        assertTrue(rowMapper.getRowMappers().isEmpty());
    }

    @SparqlResultSetMapping(name = MAPPING_NAME)
    private static final class EmptyMapping {
    }

    @Test
    public void buildMapperCreatesConstructorResultMapperWithCorrespondingVariableMappersForParameters() {
        processor.buildMapper(getMapping(WithConstructorMapping.class));
        final ResultSetMappingManager manager = processor.getManager();
        final SparqlResultMapper mapper = manager.getMapper(MAPPING_NAME);
        final ResultRowMapper rowMapper = (ResultRowMapper) mapper;
        assertEquals(1, rowMapper.getRowMappers().size());
        final ConstructorResultMapper ctorMapper = (ConstructorResultMapper) rowMapper.getRowMappers().get(0);
        assertNotNull(ctorMapper);
        assertEquals(OWLClassA.class, ctorMapper.getTargetType());
        assertEquals(3, ctorMapper.getParamMappers().size());
        assertEquals("uri", ctorMapper.getParamMappers().get(0).getName());
        assertEquals(String.class, ctorMapper.getParamMappers().get(0).getTargetType());
        assertEquals("label", ctorMapper.getParamMappers().get(1).getName());
        assertEquals(void.class, ctorMapper.getParamMappers().get(1).getTargetType());
        assertEquals("comment", ctorMapper.getParamMappers().get(2).getName());
        assertEquals(void.class, ctorMapper.getParamMappers().get(2).getTargetType());
    }

    @SparqlResultSetMapping(name = MAPPING_NAME, classes = {
            @ConstructorResult(targetClass = OWLClassA.class, variables = {
                    @VariableResult(name = "uri", type = String.class),
                    @VariableResult(name = "label"),
                    @VariableResult(name = "comment")
            })
    })
    private static final class WithConstructorMapping {
    }
}