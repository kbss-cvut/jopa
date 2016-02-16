package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Objects;
import java.util.Set;

import static cz.cvut.kbss.ontodriver.util.ErrorUtils.npxMessage;

public class SesameTypes implements Types {

    private final SesameConnection connection;
    private final SesameAdapter adapter;

    public SesameTypes(SesameConnection connection, SesameAdapter adapter) {
        this.connection = connection;
        this.adapter = adapter;
    }

    @Override
    public Set<Axiom<URI>> getTypes(NamedResource individual, URI context, boolean includeInferred)
            throws OntoDriverException {
        Objects.requireNonNull(individual, npxMessage("individual"));
        connection.ensureOpen();
        return adapter.getTypesHandler().getTypes(individual, context, includeInferred);
    }

    @Override
    public void addTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException {
        verifyValidity(individual, types);
        if (!types.isEmpty()) {
            adapter.getTypesHandler().addTypes(individual, context, types);
        }
        connection.commitIfAuto();
    }

    private void verifyValidity(NamedResource individual, Set<URI> types) {
        Objects.requireNonNull(individual, npxMessage("individual"));
        Objects.requireNonNull(types, npxMessage("types"));
        connection.ensureOpen();
    }

    @Override
    public void removeTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException {
        verifyValidity(individual, types);
        if (!types.isEmpty()) {
            adapter.getTypesHandler().removeTypes(individual, context, types);
        }
        connection.commitIfAuto();
    }
}
