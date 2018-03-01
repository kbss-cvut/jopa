package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;

import java.net.URI;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

class PropertiesHandler implements Properties {

    private final StorageConnector connector;

    PropertiesHandler(StorageConnector connector) {
        this.connector = connector;
    }

    @Override
    public Collection<Axiom<?>> getProperties(NamedResource individual, URI context, boolean includeInferred) {
        return new ExplicitAxiomLoader(connector).find(individual, context);
    }

    @Override
    public void addProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties) {
        new AxiomSaver(connector).saveAxioms(individual, properties, context);
    }

    @Override
    public void removeProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties) {
        new EpistemicAxiomRemover(connector).remove(individual, properties, context);
    }
}
