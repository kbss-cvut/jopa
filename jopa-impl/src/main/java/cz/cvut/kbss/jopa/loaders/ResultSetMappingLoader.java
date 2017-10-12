package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping;
import cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMappings;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Consumer;

/**
 * Manages {@link cz.cvut.kbss.jopa.model.annotations.SparqlResultSetMapping} instances discovered during classpath
 * processing.
 */
public class ResultSetMappingLoader implements Consumer<Class<?>> {

    private final Set<SparqlResultSetMapping> mappings = new HashSet<>();

    @Override
    public void accept(Class<?> cls) {
        final SparqlResultSetMapping mapping = cls.getDeclaredAnnotation(SparqlResultSetMapping.class);
        if (mapping != null) {
            mappings.add(mapping);
        }
        final SparqlResultSetMappings set = cls.getDeclaredAnnotation(SparqlResultSetMappings.class);
        if (set != null) {
            mappings.addAll(Arrays.asList(set.value()));
        }
    }

    public Set<SparqlResultSetMapping> getMappings() {
        return mappings;
    }
}
