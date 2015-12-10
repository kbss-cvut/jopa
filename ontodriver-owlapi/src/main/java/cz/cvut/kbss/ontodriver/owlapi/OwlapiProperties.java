package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public class OwlapiProperties implements Properties {

    private final OwlapiConnection connection;

    private final OwlapiAdapter adapter;

    public OwlapiProperties(OwlapiConnection connection, OwlapiAdapter adapter) {
        this.connection = connection;
        this.adapter = adapter;
    }

    @Override
    public Collection<Axiom<?>> getProperties(NamedResource individual, URI context, boolean includeInferred)
            throws OntoDriverException {
        Objects.requireNonNull(individual, ErrorUtils.constructNPXMessage("individual"));
        connection.ensureOpen();
        return adapter.getPropertiesHandler().getProperties(individual, includeInferred);
    }

    @Override
    public void addProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties)
            throws OntoDriverException {
        ensureValidity(individual, properties);
        if (!properties.isEmpty()) {
            adapter.getPropertiesHandler().addProperties(individual, properties);
        }
        connection.commitIfAuto();
    }

    private void ensureValidity(NamedResource individual, Map<Assertion, Set<Value<?>>> properties) {
        Objects.requireNonNull(individual, ErrorUtils.constructNPXMessage("individual"));
        Objects.requireNonNull(properties, ErrorUtils.constructNPXMessage("properties"));
        connection.ensureOpen();
    }

    @Override
    public void removeProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties)
            throws OntoDriverException {
        ensureValidity(individual, properties);
        if (!properties.isEmpty()) {
            adapter.getPropertiesHandler().removeProperties(individual, properties);
        }
        connection.commitIfAuto();
    }
}
