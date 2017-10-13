package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMappings;
import cz.cvut.kbss.jopa.model.annotations.VariableResult;
import org.junit.Test;

import static org.junit.Assert.*;

public class ResultSetMappingLoaderTest {

    private ResultSetMappingLoader loader = new ResultSetMappingLoader();

    @Test
    public void doesNothingWhenClassWithoutResultSetMappingIsPassedIn() {
        loader.accept(ResultSetMappingLoaderTest.class);
        assertTrue(loader.getMappings().isEmpty());
    }

    @Test
    public void registersSingleResultSetMappingDeclaredOnClass() {
        loader.accept(ClassWithSingleMapping.class);
        assertEquals(1, loader.getMappings().size());
        assertEquals(ClassWithSingleMapping.class.getDeclaredAnnotation(SparqlResultSetMapping.class),
                loader.getMappings().iterator().next());
    }

    @SparqlResultSetMapping(name = "testMapping", variables = {
            @VariableResult(name = "x"),
            @VariableResult(name = "y")
    })
    private static class ClassWithSingleMapping {
    }

    @Test
    public void registersAllItemsFromResultSetMappingsDeclaredOnClass() {
        loader.accept(ClassWithMappings.class);
        assertEquals(2, loader.getMappings().size());
        final SparqlResultSetMappings mappings = ClassWithMappings.class
                .getDeclaredAnnotation(SparqlResultSetMappings.class);
        for (SparqlResultSetMapping m : mappings.value()) {
            assertTrue(loader.getMappings().contains(m));
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
        loader.accept(ClassWithMappingInMappings.class);
        assertEquals(1, loader.getMappings().size());
        final SparqlResultSetMappings mappings = ClassWithMappingInMappings.class
                .getDeclaredAnnotation(SparqlResultSetMappings.class);
        for (SparqlResultSetMapping m : mappings.value()) {
            assertTrue(loader.getMappings().contains(m));
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