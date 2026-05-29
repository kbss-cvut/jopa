package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.model.annotations.NamedEntityGraph;
import cz.cvut.kbss.jopa.model.annotations.NamedEntityGraphs;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;

/**
 * Manages {@link cz.cvut.kbss.jopa.model.annotations.NamedEntityGraph} instances discovered during classpath
 * processing.
 * <p>
 * The discovered annotations are tracked together with the class on which they were declared, so that the root entity
 * type can be later resolved when the entity graph is being built.
 */
public class NamedEntityGraphLoader implements Consumer<Class<?>> {

    private final Map<Class<?>, Set<NamedEntityGraph>> graphDeclarations = new HashMap<>();

    @Override
    public void accept(Class<?> aClass) {
        final NamedEntityGraph graphDeclaration = aClass.getDeclaredAnnotation(NamedEntityGraph.class);
        if (graphDeclaration != null) {
            addDeclaration(aClass, graphDeclaration);
        }
        final NamedEntityGraphs graphDeclarationSet = aClass.getDeclaredAnnotation(NamedEntityGraphs.class);
        if (graphDeclarationSet != null) {
            for (NamedEntityGraph g : graphDeclarationSet.value()) {
                addDeclaration(aClass, g);
            }
        }
    }

    private void addDeclaration(Class<?> aClass, NamedEntityGraph declaration) {
        graphDeclarations.computeIfAbsent(aClass, k -> new HashSet<>()).add(declaration);
    }

    public Map<Class<?>, Set<NamedEntityGraph>> getGraphDeclarations() {
        return graphDeclarations;
    }
}
