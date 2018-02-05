/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.ontodriver.sesame.util.SesameUtils;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;

import java.net.URI;
import java.util.Collection;

import static org.junit.Assert.assertTrue;

public class SesameDataAccessor {

    public void persistTestData(Collection<Triple> data, EntityManager em) {
        final Repository repository = em.unwrap(Repository.class);
        try (final RepositoryConnection connection = repository.getConnection()) {
            final ValueFactory vf = connection.getValueFactory();
            connection.begin();
            for (Triple t : data) {
                if (t.getValue() instanceof URI) {
                    connection.add(vf.createIRI(t.getSubject().toString()), vf.createIRI(t.getProperty().toString()),
                            vf.createIRI(t.getValue().toString()));
                } else {
                    connection.add(vf.createIRI(t.getSubject().toString()), vf.createIRI(t.getProperty().toString()),
                            SesameUtils.createDataPropertyLiteral(t.getValue(), t.getLanguage(), vf));
                }

            }
            connection.commit();
        }
    }

    public void verifyDataPresence(Collection<Triple> data, EntityManager em) {
        final Repository repository = em.unwrap(Repository.class);
        try (final RepositoryConnection connection = repository.getConnection()) {
            final ValueFactory vf = connection.getValueFactory();
            for (Triple t : data) {
                final boolean found;
                if (t.getValue() instanceof URI) {
                    found = connection.hasStatement(vf.createIRI(t.getSubject().toString()),
                            vf.createIRI(t.getProperty().toString()), vf.createIRI(t.getValue().toString()), false);
                } else {
                    found = connection.hasStatement(vf.createIRI(t.getSubject().toString()),
                            vf.createIRI(t.getProperty().toString()),
                            SesameUtils.createDataPropertyLiteral(t.getValue(), t.getLanguage(), vf), false);
                }
                assertTrue(found);
            }
        }
    }
}
