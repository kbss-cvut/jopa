/**
 * Copyright (C) 2011 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver.Properties;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
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
