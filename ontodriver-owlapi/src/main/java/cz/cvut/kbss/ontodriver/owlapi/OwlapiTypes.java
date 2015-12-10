package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.Types;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Objects;
import java.util.Set;

public class OwlapiTypes implements Types {

    private final OwlapiConnection connection;

    private final OwlapiAdapter adapter;

    public OwlapiTypes(OwlapiConnection connection, OwlapiAdapter adapter) {
        this.connection = connection;
        this.adapter = adapter;
    }

    @Override
    public Set<Axiom<URI>> getTypes(NamedResource individual, URI context, boolean includeInferred)
            throws OntoDriverException {
        Objects.requireNonNull(individual, ErrorUtils.constructNPXMessage("individual"));
        connection.ensureOpen();
        return adapter.getTypesHandler().getTypes(individual, context, includeInferred);
    }

    @Override
    public void addTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException {
        ensureValidity(individual, types);
        if (!types.isEmpty()) {
            adapter.getTypesHandler().addTypes(individual, context, types);
        }
        connection.commitIfAuto();
    }

    private void ensureValidity(NamedResource individual, Set<URI> types) {
        Objects.requireNonNull(individual, ErrorUtils.constructNPXMessage("individual"));
        Objects.requireNonNull(types, ErrorUtils.constructNPXMessage("types"));
        connection.ensureOpen();
    }

    @Override
    public void removeTypes(NamedResource individual, URI context, Set<URI> types) throws OntoDriverException {
        ensureValidity(individual, types);
        if (!types.isEmpty()) {
            adapter.getTypesHandler().removeTypes(individual, context, types);
        }
        connection.commitIfAuto();
    }
}
