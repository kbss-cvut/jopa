package cz.cvut.kbss.jopa.query.mapper;

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
        processor.buildMapper(WithVariableMapping.getMapping());
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

        private static SparqlResultSetMapping getMapping() {
            return WithVariableMapping.class.getDeclaredAnnotation(SparqlResultSetMapping.class);
        }
    }

    @Test
    public void buildMapperBuildsEmptyMapperFromEmptyMappingConfiguration() {
        processor.buildMapper(EmptyMapping.getMapping());
        final ResultSetMappingManager manager = processor.getManager();
        final SparqlResultMapper mapper = manager.getMapper(MAPPING_NAME);
        assertNotNull(mapper);
        final ResultRowMapper rowMapper = (ResultRowMapper) mapper;
        assertEquals(MAPPING_NAME, rowMapper.getName());
        assertTrue(rowMapper.getRowMappers().isEmpty());
    }

    @SparqlResultSetMapping(name = MAPPING_NAME)
    private static final class EmptyMapping {

        private static SparqlResultSetMapping getMapping() {
            return EmptyMapping.class.getDeclaredAnnotation(SparqlResultSetMapping.class);
        }
    }
}