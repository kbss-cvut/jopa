package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import cz.cvut.kbss.ontodriver.jena.util.Procedure;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

class JenaProperties implements Properties {

    private final JenaAdapter adapter;

    private final Procedure beforeCallback;
    private final Procedure afterChangeCallback;

    JenaProperties(JenaAdapter adapter, Procedure beforeCallback, Procedure afterChangeCallback) {
        this.adapter = adapter;
        this.beforeCallback = beforeCallback;
        this.afterChangeCallback = afterChangeCallback;
    }

    @Override
    public Collection<Axiom<?>> getProperties(NamedResource individual, URI context, boolean includeInferred)
            throws JenaDriverException {
        Objects.requireNonNull(individual);
        beforeCallback.execute();
        return adapter.propertiesHandler().getProperties(individual, context, includeInferred);
    }

    @Override
    public void addProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties)
            throws JenaDriverException {
        Objects.requireNonNull(individual);
        Objects.requireNonNull(properties);
        beforeCallback.execute();
        adapter.propertiesHandler().addProperties(individual, context, properties);
        afterChangeCallback.execute();
    }

    @Override
    public void removeProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties)
            throws JenaDriverException {
        Objects.requireNonNull(individual);
        Objects.requireNonNull(properties);
        beforeCallback.execute();
        adapter.propertiesHandler().removeProperties(individual, context, properties);
        afterChangeCallback.execute();
    }
}
