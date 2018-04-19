package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.jena.util.Procedure;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Objects;
import java.util.Set;

public class JenaTypes implements Types {

    private final Procedure beforeCallback;
    private final Procedure afterCallback;

    private final JenaAdapter adapter;

    JenaTypes(JenaAdapter adapter, Procedure beforeCallback, Procedure afterCallback) {
        this.beforeCallback = beforeCallback;
        this.afterCallback = afterCallback;
        this.adapter = adapter;
    }

    @Override
    public Set<Axiom<URI>> getTypes(NamedResource individual, URI context, boolean includeInferred)
            throws OntoDriverException {
        Objects.requireNonNull(individual);
        beforeCallback.execute();
        return adapter.typesHandler().getTypes(individual, context, includeInferred);
    }

    @Override
    public void addTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException {
        Objects.requireNonNull(individual, "individual");
        Objects.requireNonNull(types, "types");
        beforeCallback.execute();
        adapter.typesHandler().addTypes(individual, context, types);
        afterCallback.execute();
    }

    @Override
    public void removeTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException {
        Objects.requireNonNull(individual, "individual");
        Objects.requireNonNull(types, "types");
        beforeCallback.execute();
        adapter.typesHandler().removeTypes(individual, context, types);
        afterCallback.execute();
    }
}
