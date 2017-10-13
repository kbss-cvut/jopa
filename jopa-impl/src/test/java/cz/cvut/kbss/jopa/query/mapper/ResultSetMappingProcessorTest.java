package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.model.annotations.VariableResult;
import cz.cvut.kbss.jopa.query.ResultSetMappingManager;
import org.junit.Test;

import java.net.URI;

import static org.junit.Assert.assertNotNull;

public class ResultSetMappingProcessorTest {

    private static final String MAPPING_NAME = "testMapping";

    private ResultSetMappingProcessor processor = new ResultSetMappingProcessor();

    @Test
    public void buildMapperCreatesRowMapperWithVariableMappersConfiguredInMappingAnnotation() {
        processor.buildMapper(WithVariableMapping.getMapping());
        final ResultSetMappingManager manager = processor.getManager();
        final SparqlResultMapper mapper = manager.getMapper(MAPPING_NAME);
        assertNotNull(mapper);
        // TODO Verify variable mappers
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
}