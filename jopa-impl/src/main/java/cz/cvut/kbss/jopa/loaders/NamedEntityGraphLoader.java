package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.model.annotations.NamedEntityGraph;
import cz.cvut.kbss.jopa.model.annotations.NamedEntityGraphs;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Consumer;

/**
 * Manages {@link cz.cvut.kbss.jopa.model.annotations.NamedEntityGraph} instances discovered during classpath
 * processing.
 */
public class NamedEntityGraphLoader implements Consumer<Class<?>> {

    private final Set<NamedEntityGraph> graphDeclarations = new HashSet<>();

    @Override
    public void accept(Class<?> aClass) {
        final NamedEntityGraph graphDeclaration = aClass.getDeclaredAnnotation(NamedEntityGraph.class);
        if (graphDeclaration != null) {
            graphDeclarations.add(graphDeclaration);
        }
        final NamedEntityGraphs graphDeclarationSet = aClass.getDeclaredAnnotation(NamedEntityGraphs.class);
        if (graphDeclarationSet != null) {
            graphDeclarations.addAll(Arrays.asList(graphDeclarationSet.value()));
        }
    }

    public Set<NamedEntityGraph> getGraphDeclarations() {
        return graphDeclarations;
    }
}
