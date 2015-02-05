package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.Properties;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.Axiom;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;
import org.openrdf.model.ValueFactory;

import java.net.URI;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

/**
 * @author ledvima1
 */
class SesameProperties implements Properties {

    private final Connector connector;
    private final ValueFactory valueFactory;
    private final String language;

    private final SesameConnection connection;

    public SesameProperties(SesameConnection connection, SesameAdapter adapter) {
        this.connection = connection;
        this.connector = adapter.getConnector();
        this.valueFactory = adapter.getValueFactory();
        this.language = adapter.getLanguage();
    }

    @Override
    public Collection<Axiom<?>> getProperties(NamedResource individual, URI context, boolean includeInferred)
            throws SesameDriverException {
        connection.ensureOpen();
        return new AxiomLoader(connector, valueFactory).loadAxioms(individual, includeInferred, context);
    }

    @Override
    public void addProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties)
            throws OntoDriverException {
        connection.ensureOpen();
        new AxiomSaver(connector, valueFactory, language).persistAxioms(individual, properties, context);
        connection.commitIfAuto();
    }

    @Override
    public void removeProperties(NamedResource individual, URI context, Map<Assertion, Set<Value<?>>> properties)
            throws OntoDriverException {
        new EpistemicAxiomRemover(connector, valueFactory, language).remove(individual, properties, context);
        connection.commitIfAuto();
    }
}
